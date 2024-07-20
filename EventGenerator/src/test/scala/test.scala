import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import java.io.File

/*
	Then just type while in /home/kost (or another user):

	$ dot -Tps graph<i>.dot -o out<i>.ps  // for i = (1,n)
	$ evince out<i>.ps &
*/


class Test extends FunSuite with BeforeAndAfter {

	before {
    	new File("./output/").mkdir
  	}

	test("ordinary cases") { 

		val set  = scala.collection.mutable.Set(1, 2, 3, 4, 5, 6)

		Generator.makeChains(
						1, 
						set, 
						"./output/graph1.dot")
	
		Generator.makeChains(
						3, 
						set, 
						"./output/graph2.dot")

		Generator.makeChains(
						5, 
						set, 
						"./output/graph3.dot")
	}

	test("trying to generate zero-len chain") {

		val set = scala.collection.mutable.Set(1)

		intercept[IllegalArgumentException] {

			Generator.makeChains(
						0, 
						set, 
						"./output/failed.dot")
		}
	}

	test("Generator.generateEvent генерирует уникальные события") {
		val set = Set(1,2,3)
		val t = 0
		val src = 0
		val events = for(i <- 0 to 100) yield Generator.generateEvent(t, src, set.toVector)
		assert(events.forall(e1 => events.filter(e2 => e1 == e2).length == 1))
	}
}
