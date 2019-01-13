package org.asarkar.practice

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class OptimalBSTSpec extends FlatSpec with TableDrivenPropertyChecks {
  "OptimalBST" should "compute minimum-possible average search time" in {
    val data =
    // format: off
      Table(("items", "cost"),
        (IndexedSeq[Double](4, 2, 6, 3), 26d),
        (IndexedSeq(.05, .4, .08, .04, .1, .1, .23), 2.18),
        (IndexedSeq(.20, .05, .17, .10, .20, .03, .25), 2.23)
      )
    // format: on

    forAll(data) { (items, cost) =>
      OptimalBST.cost(items) shouldBe (cost +- 0.01d)
    }
  }
}
