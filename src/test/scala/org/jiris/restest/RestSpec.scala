package org.jiris.restest

import org.scalatest.WordSpec
import org.scalatest.Matchers

class RestSpec extends WordSpec with Rest with Matchers {
  "GET" should {
    "not fail" in {
      get"http://www.google.com"
    }

    "return 200" in {
      get"http://www.google.com"
      $code shouldBe 200
    }
  }
}