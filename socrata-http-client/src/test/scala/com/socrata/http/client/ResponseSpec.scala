package com.socrata.http.client

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.PropertyChecks
import java.io.Reader
import javax.activation.{MimeTypeParseException, MimeType}
import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.testsupport.ArbitraryJValue
import com.rojoma.json.v3.io.{CompactJsonWriter, JValueEventIterator}
import org.scalacheck.Arbitrary

class ResponseTest extends FunSuite with MustMatchers {
  test("Mimetype matching works as expected") {
    List(
      ("foo/bar", "foo/bar", true),
      ("foo/bar", "foo/bar+baz", true),
      ("foo/bar+baz", "foo/bar+baz", true),
      ("foo/bar+baz", "foo/bar", false),
      ("goo/bar", "foo/bar", false),
      ("application/json", "application/json+cjson", true),
      ("application/json+cjson", "application/json", false),
    ).foreach { case (pattern, candidate, matches) =>
        println(pattern, candidate, matches)
        Response.matches(new MimeType(pattern), new MimeType(candidate)) must be (matches)

    }

  }
}
