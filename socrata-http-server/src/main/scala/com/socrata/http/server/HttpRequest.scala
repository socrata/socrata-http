package com.socrata.http.server

import com.rojoma.simplearm.v2.ResourceScope

import scala.collection.JavaConverters._
import java.net.URLDecoder
import javax.servlet.http.HttpServletRequest

import com.socrata.http.common.util.{ContentNegotiation, HttpUtils, CharsetFor}
import com.socrata.http.server.util.PreconditionParser
import org.joda.time.DateTime

trait HttpRequest {
  def servletRequest: HttpServletRequest
  def resourceScope: ResourceScope
}

class WrapperHttpRequest(val underlying: HttpRequest) extends HttpRequest {
  def servletRequest = underlying.servletRequest
  def resourceScope = underlying.resourceScope
}

object HttpRequest {
  implicit def augmentedHttpRequest(req: HttpRequest) = new AugmentedHttpRequest(req)

  // This will allow us to add more (stateless) methods to HttpRequest without braking binary compatibility
  final class AugmentedHttpRequest(val `private once 2.10 is no longer a thing`: HttpRequest) extends AnyVal {
    private def self = `private once 2.10 is no longer a thing`
    private def servletRequest = self.servletRequest

    def hostname =
      header("X-Socrata-Host").orElse(header("Host")).getOrElse("").split(':').head

    def requestPath: List[String] = // TODO: strip off any context and/or servlet path
      requestPathStr.split("/", -1 /* I hate you, Java */).iterator.drop(1).map(URLDecoder.decode(_, "UTF-8")).toList

    def requestPathStr = servletRequest.getRequestURI

    def queryStr = servletRequest.getQueryString

    def method: String = servletRequest.getMethod

    def header(name: String): Option[String] =
      Option(servletRequest.getHeader(name))

    def headerNames: Iterator[String] =
      checkHeaderAccess(servletRequest.getHeaderNames).asScala

    def headers(name: String): Iterator[String] =
      checkHeaderAccess(servletRequest.getHeaders(name)).asScala

    def contentType = Option(servletRequest.getContentType)

    /* Sets the request's character encoding based on its content-type.
     * Use this before calling `getReader`!  This is especially important
     * when reading JSON! */
    def updateCharacterEncoding(): Option[CharsetFor.ContentTypeFailure] =
      contentType.flatMap { ct =>
        CharsetFor.contentType(ct) match {
          case CharsetFor.Success(cs) =>
            servletRequest.setCharacterEncoding(cs.name)
            None
          case f: CharsetFor.ContentTypeFailure =>
            Some(f)
        }
      }

    def accept = headers("Accept").toVector.flatMap(HttpUtils.parseAccept)
    def acceptCharset = headers("Accept-Charset").toVector.flatMap(HttpUtils.parseAcceptCharset)
    def acceptLanguage = headers("Accept-Language").toVector.flatMap(HttpUtils.parseAcceptLanguage)

    def precondition = PreconditionParser.precondition(self)

    def lastModified: Option[DateTime] = dateTimeHeader("Last-Modified")

    def dateTimeHeader(name: String, ignoreParseErrors: Boolean = true): Option[DateTime] = {
      try {
        header(name).map(HttpUtils.parseHttpDate)
      } catch {
        // In most cases, if an incorrectly-formatted date header is set, HTTP 1.1 dictates to simply ignore it
        case ex: IllegalArgumentException if ignoreParseErrors =>
          None
      }
    }

    def negotiateContent(implicit contentNegotiation: ContentNegotiation) = {
      val filename = requestPath.last
      val dotpos = filename.lastIndexOf('.')
      val ext = if(dotpos >= 0) Some(filename.substring(dotpos + 1)) else None
      contentNegotiation(accept.toSeq, contentType, ext, acceptCharset.toSeq, acceptLanguage.toSeq)
    }

    private def checkHeaderAccess[T <: AnyRef](result: T): T = {
      if(result eq null) throw new IllegalStateException("Container does not allow access to headers")
      result
    }
  }
}