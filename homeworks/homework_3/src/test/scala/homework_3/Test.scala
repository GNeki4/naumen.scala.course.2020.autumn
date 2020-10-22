package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_example - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
        }

        'test_prettyBooleanFormatter - {
            val check = (arg: Any, res: String) => {
                assert(Exercises.prettyBooleanFormatter1(arg) == res)
                assert(Exercises.prettyBooleanFormatter2(arg) == res)
                assert(Exercises.prettyBooleanFormatter3(arg) == res)
            }
            check(true, "правда")
            check(false, "ложь")
            check(Seq(1, 2, 3), "List(1, 2, 3)")
            check(Map(1 -> 1, 2 -> 2, 3 -> 3), "Map(1 -> 1, 2 -> 2, 3 -> 3)")
        }

        'test_max - {
            val check = (arg: Seq[Int], res1: Int, res2: Seq[Int], res3: Option[Int]) => {
                assert(Exercises.max1(arg) == res1)

                assert(
                    try {
                        val res = Exercises.max2(arg)
                        true
                    } catch{
                        case ex: Exception => true
                        case _ => false
                    }
                    )

                assert(Exercises.max3(arg) == res3)
            }

            check(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 10, Seq(10), Option(10))
            check(Seq(-1, -2, -3), -1, Seq(-1), Some(-1))
            check(Seq(), 0, Seq(0), None)
        }

        'test_sum - {
            val check = (x: Int, y: Int, res: Int) => {
                assert(Exercises.sum1(x, y) == res)
                assert(Exercises.sum2(x, y) == res)
                assert(Exercises.sum3(x, y) == res)
            }

            check(100, 200, 300)
            check(-100, 100, 0)
            check(0, 0, 0)
        }
    }
}
