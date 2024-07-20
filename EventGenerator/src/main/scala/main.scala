package ru.ispras.dst

import  scala.collection.mutable.Set
import  scala.collection.mutable.ListBuffer
import  scala.io.StdIn.readInt

import  org.jgrapht._
import  org.jgrapht.graph._
import  org.jgrapht.ext.DOTExporter
import  org.jgrapht.ext.IntegerNameProvider
import  org.jgrapht.ext.StringNameProvider
import  org.jgrapht.ext.StringEdgeNameProvider

import  java.io.PrintWriter
import  java.lang.IllegalArgumentException


case class Event(t: Int, src: Int, dst: Int, desc: Map[Int, String])

case class LabeledVertex(id: Int, label: String)

class LabeledEdge(new_v1: LabeledVertex, new_v2: LabeledVertex, new_label: String) extends DefaultWeightedEdge {

    private var _v1: LabeledVertex = new_v1
    private var _v2: LabeledVertex = new_v2
    private var _label: String = new_label


    def v1: LabeledVertex = _v1

    def v2: LabeledVertex = _v2

    def label: String = _label

    def label_=(label: String) = {

        _label = label
    }

    override def toString: String = _label
}


object Generator extends App {

	var graph_id = 0

	def inputNodeIdentifiers(): Set[Int] = {

		var input = Set[Int]()
		var value = readInt()

		while (value != 0) {

			input += value
			value = readInt()
		}

		return input
	}

	/**
	 *  Gets random in range [N,M]
	 *
	 *  Inclusive.
	 *  Deals with Int, values must be non-negative, 
	 *  second must be greater or equal to first.
	 */

	def getRandomNM(N: Int, M: Int): Int = {

		if (N < 0 || M < 0) {

			throw new IllegalArgumentException(
						"One of the arguments is negative")
		}

		if (N > M) {

			throw new IllegalArgumentException(
						"First value is greater than second")
		}

		val rand = scala.util.Random

		return N + rand.nextInt(M-N+1)
	}

	def getRandomString(max_length: Int): String = {

		var rand = scala.util.Random
		var output: String = ""
		var i = 1 + rand.nextInt(max_length)

		while (i > 0) {

				output += rand.nextPrintableChar()
				i -= 1
		}

		return output
	}

	def generateEvent(
		time: Int, 
		src_prev: Int, 
		ident_vector: Vector[Int]): Event = {

		var desc = collection.mutable.Map[Int, String]()
		val desc_entries = getRandomNM(1, 10)
		var i = 0

		while (i < desc_entries) {

			val rand_key: Int = getRandomNM(0, 1000)
			val rand_string: String = getRandomString(10)

			if (!desc.contains(rand_key)) {

				desc += (rand_key -> rand_string)
				i += 1
			}
		}

		var generated = new Event(
				time, 
				src_prev, 
				ident_vector(getRandomNM(0, ident_vector.size-1)), 
				desc.toMap)

		return generated
	}

	def generateNode(
        graph: DefaultDirectedWeightedGraph[LabeledVertex, LabeledEdge], 
		ident_vector: Vector[Int], 
		prev_vertex: LabeledVertex, 
		time: Int, 
		entry: Int): LabeledVertex = {

		val event = generateEvent(
						time, 
						(prev_vertex.label).toInt, 
						ident_vector)
		val vertex = new LabeledVertex(
						graph_id, 
						(event.dst).toString)

		if (entry > 0) {

			graph.addVertex(vertex)
            graph.addEdge(prev_vertex, vertex, new LabeledEdge(prev_vertex, vertex, time.toString))
			graph_id += 1

			// for possible branching
			if (getRandomNM(0, 1) > 0) {

				generateNode(
					graph,
					ident_vector,
					vertex,
					time+1,
					entry-1)
			}

			return generateNode(
						graph, 
						ident_vector, 
						vertex, 
						time+1, 
						entry-1)
		}

		else null
	}

	def generateTree(
        graph: DefaultDirectedWeightedGraph[LabeledVertex, LabeledEdge], 
		ident_vector: Vector[Int], 
		depth: Int): Unit = {

		// root
		val vec_size = ident_vector.size
		val first_unit = ident_vector(getRandomNM(0, vec_size-1))
		val event = generateEvent(
						0, 
						first_unit, 
						ident_vector)
		val vertex = new LabeledVertex(
						graph_id, 
						first_unit.toString)

		graph.addVertex(vertex)
		graph_id += 1

		// for possible branching
		if (getRandomNM(0, 1) > 0) {

			generateNode(
				graph, 
				ident_vector, 
				vertex, 
				1, 
				depth-1)
		}

		generateNode(
			graph, 
			ident_vector, 
			vertex, 
			1, 
			depth-1)
	}

	def makeChains(
		max_depth: Int, 
		ident_set: Set[Int], 
		output_file: String) = {


		if (max_depth <= 0) {

			throw new IllegalArgumentException(
						"Depth must be positive")
		}

		val writer = new PrintWriter(output_file)
        val graph = new DefaultDirectedWeightedGraph[LabeledVertex, LabeledEdge](classOf[LabeledEdge])

		val vertexId = 
			new IntegerNameProvider[LabeledVertex]() {
	            def getVertexName(v: LabeledVertex): String = {

					return (v.id).toString
				}
			}

		val vertexLabel = 
			new StringNameProvider[LabeledVertex]() {
	            override def getVertexName(v: LabeledVertex): String = {

					return v.label
				}
			}

        val edgeLabel = 
            new StringEdgeNameProvider[LabeledEdge]()

		val exporter = 
            new DOTExporter[LabeledVertex, LabeledEdge](
			vertexId, 
			vertexLabel,
            edgeLabel)

		generateTree(graph, ident_set.toVector, max_depth)
		exporter.exportGraph(graph, writer)
		writer.flush
		writer.close
	}
}
