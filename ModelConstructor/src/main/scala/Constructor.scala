package ru.ispras.dst

import  scala.collection.mutable.ListBuffer
import  scala.collection.mutable.Set

import  scala.xml._

import  org.jgrapht._
import  org.jgrapht.graph._
import  org.jgrapht.ext.DOTExporter
import  org.jgrapht.ext.IntegerNameProvider
import  org.jgrapht.ext.StringNameProvider
import  org.jgrapht.ext.StringEdgeNameProvider

import  java.io.PrintWriter


case class LabeledVertex(id: Int, label: String)


class LabeledEdge(new_v1: LabeledVertex, new_v2: LabeledVertex, new_label: String)
    extends DefaultWeightedEdge {

    private var _v1: LabeledVertex = new_v1
    private var _v2: LabeledVertex = new_v2
    private var _label: String = new_label


    def v1: LabeledVertex = _v1

    def v2: LabeledVertex = _v2

    def label: String = _label

    def label_=(label: String) = {_label = label}

    override def toString: String = _label
}


class Node(content: String) {

    private var _children_list  = List[Int]()  // indices in the next layer
    private var _reference_list = List[Int]()  // indices of states (nodes) in a route

    private val _content = content


    def addChild(index: Int) = { _children_list :+= index }
    def addReference(index: Int) = { _reference_list :+= index }
    def getChildren: List[Int] = _children_list
    def getRefs: List[Int] = _reference_list
    def getContent: String = _content
}


class TreeLike(max_depth: Int) {

    val _max_depth = max_depth
    // there will be stored all of diagram nodes by layers
    var nodes = new ListBuffer[ListBuffer[Node]]

    // creating buffers for all layers
    for (i <- 1 to _max_depth)
        nodes += new ListBuffer[Node]


    // assume that process has at least one state
    def initWithProcess(states: ListBuffer[String], 
                        loops:  ListBuffer[Set[Int]]) {

        
        var prev_node = new Node(states(0))


        // if first state has references, it may be only reference to itself
        if (!loops(0).isEmpty)
            prev_node.addReference(0)

        nodes(0) += prev_node

        for (i <- 1 to (states.size-1)) {

            val current_node = new Node(states(i))

            prev_node.addChild(0)
            loops(i).foreach(current_node.addReference(_))
            nodes(i) += current_node
            prev_node = current_node
        }
    }


    // Returns list of Int describing sequence of 
    // layer number (as index) and element id in that layer while passing by same states.
    // Last pair defines node which we have to connect to the new branch.
    // Must be used ONLY for initialised TreeLike and processes with states.size >= 2
    def findPrefixTrace(states: ListBuffer[String]): List[Int] = {

        // starts with zero for 0-layer which always has only one element
        var layer_index = 0
        var trace = List[Int](0)

        for (i <- 1 to (states.size-1)) {

            val state = states(i)
            var found = false

            nodes(i-1)(layer_index).getChildren.foreach {child_idx => 
            
                if (nodes(i)(child_idx).getContent == state) {

                    layer_index = child_idx
                    trace :+= layer_index
                    found = true
                }
            }

            if (!found) return trace
        }

        // If we've reached here, route has been constructed for states <states> =>
        // we have process that is longer that any presented in nodes but 
        // repeats the same state sequence.
        // Weird situation because in our model 
        // every process must be represented by separated 
        // route in the graph => so returning -1

        // XXX ??? XXX //
        println("WEIRD SITUATION")
        List[Int](-1)
    }


    // note that we don't have to check for similar processes: 
    // we have filtered such processes earlier

    // and, of course, we assuming that first state is similar
    // for all analyzed process
    def addProcess(states: ListBuffer[String], 
                   loops:  ListBuffer[Set[Int]]) = {

        val trace = findPrefixTrace(states)

        // adding references for those elements which 
        // are fixed one more time but not checked
        // for back references
        for (i <- trace.indices) {

            loops(i).foreach{nodes(i)(trace(i)).addReference(_)}
        }

        // afterfork represents layer number from which (inclusive) we are 
        // starting adding states for a new distinct branch
        val afterfork = trace.size
        var prev_node = new Node(states(afterfork))

        nodes(afterfork) += prev_node
        // adding new child to a splitting node
        nodes(afterfork-1)(trace.last).addChild(nodes(afterfork).size-1)
        loops(afterfork).foreach(prev_node.addReference(_))

        for (i <- (afterfork+1) to (states.size-1)) {

            val current_node = new Node(states(i))

            nodes(i) += current_node
            prev_node.addChild(nodes(i).size-1)
            loops(i).foreach(current_node.addReference(_))
            prev_node = current_node
        }
    }


    // NOTE this function can not represent true order of the elements 
    // in layers; it just prints all the states in the order they were added
    def printLayers = {

        for (i <- (0 to nodes.size-1)) {

            nodes(i).foreach(node => print(node.getContent + " "))
            println
        }
    }


    // init_route represents a set of (vertex, id_in_layer) until
    // the vertex (inclusively) of splitting.
    // child_index represents the index which we have to choose 
    // in init_route.last - related node for moving further.
    // Be sure that init_route.last element prototype has the child
    // with index child_index
    def findLeftTraverse(graph: DefaultDirectedGraph[LabeledVertex, LabeledEdge],
                         init_route:  ListBuffer[(LabeledVertex, Int)], 
                         child_index: Int, 
                         vertex_id: Int): ListBuffer[(LabeledVertex, Int)] = {
       
        var route = init_route

        // i2g = index-to-go
        var i2g   = child_index
        var vid   = vertex_id

//        //println("i2g: " + i2g + "; route: " + route.toString)


        // First iteration is fixed: we're going from last element of init_route
        // by the index child_index.
        // But more interesting are another iterations - for them exists such condition in loop
        while (!nodes(route.size-1)(route.last._2).getChildren.isEmpty) {

            val next_node = nodes(route.size)(i2g)
            val vertex = new LabeledVertex(vid, next_node.getContent)
            val pair = (vertex, i2g)
            graph.addVertex(vertex)
            graph.addEdge(route.last._1, vertex, new LabeledEdge(route.last._1, vertex, ""))
            route += pair
            vid   += 1


            //println("route at iteration: " + route.toString)

            // XXX NEED DISTINCT? XXX
            next_node.getRefs.distinct.foreach {
                index => 
                //println("REF INDEX: " + index)
                graph.addEdge(vertex, route(index)._1, new LabeledEdge(vertex, route(index)._1, ""))
            }

            if (!next_node.getChildren.isEmpty)
                i2g = next_node.getChildren.min

            // else we don't need to modify i2g cause
            // next iteration just won't go
        }

        route
    }



    // exports info about processes represented in this class to
    // the pre-graphical format
    def injectTreeToGraph(graph: DefaultDirectedGraph[LabeledVertex, LabeledEdge], 
                          vertex_id: Int): Int = {

        var vid = vertex_id
        val root  = nodes(0)(0)
        val root_vertex = new LabeledVertex(vid, root.getContent)

        graph.addVertex(root_vertex)

        if (!root.getRefs.isEmpty)
            graph.addEdge(root_vertex, root_vertex, new LabeledEdge(root_vertex, root_vertex, ""))

        vid += 1

        // for first we'll process trivial case, 
        // in which we have only one vertex in a tree
        if (root.getChildren.isEmpty) {

            return (vid + 1)
        }

        // else

        var route = new ListBuffer[(LabeledVertex, Int)]
        var size_diff = 0

        val root_pair = (root_vertex, 0)

        route += root_pair
        size_diff = route.size
        route = findLeftTraverse(graph, route, root.getChildren.min, vid)
        size_diff = route.size - size_diff
        vid += size_diff

        // now we are moving through the tree in pre-order way 
        // visiting every right branch of the left most traverse
        // by moving from bottom to the root

        // just layer on which we're standing
        var layer = route.size-1

        // i.e. until all possible traverses are not processed
        while (route.size != 1) {

            val brothers = nodes(layer-1)(route(layer-1)._2).getChildren
            //println(brothers.toString)

            // if we have done all the children for this parent
            if (brothers.filter(_ > route(layer)._2).isEmpty) {

                route = route.dropRight(1)
                layer -= 1
            }

            else {


                //println(brothers.filter(_ > route(layer)._2).min)

                size_diff = route.size
                route = findLeftTraverse(graph, 
                                         route.dropRight(1), 
                                         brothers.filter(_ > route(layer)._2).min, 
                                         vid)
                layer = route.size-1
                size_diff = route.size - size_diff
                vid += size_diff
            }
        }

        vid
    }
}



object Constructor extends App {


    /**
     *  Checks whether the <event> 
     *  belongs to the process described 
     *  by <exact> information.
     *
     *  Usability goes from the convention
     *  that out event log consists of 
     *  notes about events, where three 
     *  values characterize certain process, 
     *  which event we're processing.
     */

    def haveMatches(event: MnpEvent, exact: ListBuffer[String]): Boolean = {

        // additional checks for nonzero string is needed because
        // "" == "" is True, but in our case it is incorrect

        if ((event.requestId == exact(0) && exact(0) != "") || 
            (event.processId == exact(1) && exact(1) != "") || 
            (event.npId == exact(2) && exact(2) != "")) {

            true
        }

        else false
    }

    /**
     *  Checks whether the <event> belongs to
     *  the process that hasn't present in
     *  a process pool <data> yet.
     *
     *  In case of the new process, 
     *  returns -1; if <event> is 
     *  related to some fixed process, 
     *  returns index of that process 
     *  in <data>
     */

    def isProcessNew(event: MnpEvent, 
                     data: ListBuffer[ListBuffer[String]]): Int = {

        val pattern: ListBuffer[String] = ListBuffer[String](event.requestId, 
                                                             event.processId, 
                                                             event.npId)
        data.indexWhere {state_list => state_list.slice(0, 3) == pattern}

    }

    /**
     *  Fills all posible info about 
     *  process described by <info>
     *  by extracting some additional 
     *  values from <event>.
     *
     *  Note that we don't need to 
     *  replace adequate info by 
     *  empty values.
     *
     */

    def fillInfo(event: MnpEvent, 
                 info: ListBuffer[String]): Unit = {

        if (info(0) == "") {info(0) = event.requestId}
        if (info(1) == "") {info(1) = event.processId}
        if (info(2) == "") {info(2) = event.npId}
    }

    /**
     *  Reduces loops in the given 
     *  sequence <list>. Also gives information
     *  about loops in additional data structure <loops_info>.
     *
     *  Returns modified <result> instead of original <list>
     *  and info about loops - <loops_info>. Last one is a 
     *  list of sets, where index relates to the index in <result>
     *  and appropriate set[Int] describes elements referred by
     *  current element index we inspect. For instance, in the 
     *  case of alteration such:
     *
     *  a b (c)(c) a b c  ==>  (a b c)(a b c) ==>  a b c 
     *
     *  at the final stage we'll receive 
     *  <loops_info> == [Set(), Set(), Set(0,2)]
     */

    def reduceLoops(list: ListBuffer[String]) = {

        var loops_info: ListBuffer[Set[Int]] = new ListBuffer()
        var result = new ListBuffer[String]()
        var size = list.size
        var i = 0

        result = list

        // creating the empty-set-filled additioanl information list
        while (i < size) {

            loops_info += scala.collection.mutable.Set[Int]()
            i += 1
        }

        i = 0

        while (i < size) {

            size = result.size

            val initial = result(i)
            var j = i + 1

            while ((j < size) && (initial != result(j)))
                j += 1

            
            // reached the end, further scanning
            if (j == size)
                i += 1

            // found first element match
            else {

                val pattern = result.slice(i, j)
                val pattern_size = j - i
                var from = j
                var repeats = 0

                // moving till the end and detecting continious sequence
                // of pieces similar to <pattern>
                while (from < size) {

                    // condition for continuity (if != from then it may be far
                    // away from the initial i-position, e.g. abc<trash>[abc])
                    if (result.indexOfSlice(pattern, from) == from) {

                        var k = 0

                        // checking loopbacks in current inspecting pattern
                        while (k < pattern_size) {

                            if (loops_info(from+k).nonEmpty) {

                                // translating detected loopbacks to pattern-related indices
                                loops_info(from+k) = loops_info(from+k).map {ref => 

                                                                             if (ref >= from) 
                                                                                (i + ref - from)

                                                                             else if (ref >= i && ref < from) {

                                                                                var diff = 0

                                                                                while (ref-diff >= i+pattern_size)
                                                                                    diff += pattern_size

                                                                                ref - diff
                                                                             }  

                                                                             else ref }

                                // union existing loopbacks in start pattern with found
                                loops_info(i+k) = loops_info(i+k).union(loops_info(from+k))

                                // and not forgetting to clear the mapped set - we won't have such element!
                                loops_info(from+k).clear
                            }

                            k += 1
                        }

                        repeats += 1
                        from    += pattern_size
                    }

                    // for synthetic breaking the loop (cause continuity os broken)
                    else from += size
                }

                // not found similar => moving further with another pattern
                if (repeats == 0) {
                    i += 1
                }

                else {

                    // leaves only first pattern occurrence
                    result.remove(j, pattern_size*repeats)
                    // reduces <loops_info> to appropriate number of elements
                    loops_info = loops_info.take(result.size)

                    // registering loop from the end of pattern to its beginning
                    loops_info(i+pattern_size-1) += i

                    // go back and check possible new loops
                    // (we need to return because of possible new-instanted loops)
                    i = 0
                }
            }
        }

        (result, loops_info)
    }


    // At success, returns two values:
    // index of splitting point and list of indices
    // representing subset of initial set of indices 
    // according to the states in the new (distinct)
    // process.
    // In the case of process w/o references ahead, 
    // we just returning (-1, List())

    def detectSplit(loops_info: ListBuffer[Set[Int]]): 
                                (Int, List[Int]) = {

        var split_point  = -1
        var reference_to = -1

        for (i <- loops_info.indices) {

            loops_info(i).foreach {
            
                ref =>

                // if have reference to element after current one
                // and not found any yet
                if ((ref > i) && 
                    (split_point == -1)) {

                    split_point  = i
                    reference_to = ref
                }
            }
        }

        // if we have to split
        if (split_point != -1) {

            var branch  = List[Int]()

            for (i <- 0 to split_point)
                branch :+= i

            for (i <- reference_to to loops_info.size-1)
                branch :+= i

            (split_point, branch)
        }

        
        else (-1, List[Int]())
    }


    // Returns modified process_pool
    def reduceAheadRefs(process_pool: ListBuffer[(ListBuffer[String], ListBuffer[Set[Int]])]):
                                      ListBuffer[(ListBuffer[String], ListBuffer[Set[Int]])] = {

        var modified_pool = process_pool
        var index = 0

        while (index < modified_pool.size) {

            val (split_point, index_mask) = detectSplit(modified_pool(index)._2)

            // if we need to split
            if (split_point != -1) {

                val states = modified_pool(index)._1
                val info   = modified_pool(index)._2

                var new_states = ListBuffer[String]()
                var new_info   = ListBuffer[Set[Int]]()

                for (i <- index_mask) {

                    new_states += states(i)
                    new_info   += info(i)
                }

                // deleting ahead reference in the initial process
                modified_pool(index)._2(split_point) = info(split_point).diff(Set(index_mask(split_point+1)))
                // and in the descendant
                new_info(split_point) = new_info(split_point).diff(Set(index_mask(split_point+1)))

                // by considering process length, we can analyze
                // similarity to the initial:
                // in the case of splitting we can receive process with 
                // the same number of states if and only if there was
                // a simple reference ahead to the next element => 
                // we deal with similar process
                if (new_states.size != states.size)
                    modified_pool.insert(index+1, (new_states, new_info))
            }

            else index += 1
        }

        modified_pool
    }


    /**
     * Includes given portion of info into 
     * the graph <graph>.
     *
     * <state_list> represents states of a process.
     * <loops_info> contains info about back-references to vertices.
     * Sic, DefaultDirectedGraph w/o "Weighted" - because it is superclass.
     * Returns modified vertex_id (increased because of added vertices).
     */

    def injectToGraph(graph: DefaultDirectedGraph[LabeledVertex, LabeledEdge], 
                      state_list: ListBuffer[String], 
                      loops_info: ListBuffer[Set[Int]], 
                      vertex_id: Int): Int = {

        var vertices_list = new ListBuffer[LabeledVertex]()
        var vid = vertex_id
        var i = 0

        val first_vertex = new LabeledVertex(vid, state_list(0))

        vertices_list += first_vertex
        graph.addVertex(first_vertex)

        // in case of first vertex has references to itself
        if (loops_info(0).nonEmpty) {
            graph.addEdge(first_vertex, first_vertex, new LabeledEdge(first_vertex, first_vertex, ""))
        }

        vid += 1

        for (i <- (1 to state_list.size-1)) {

            val vertex = new LabeledVertex(vid, state_list(i))

            vertices_list += vertex
            graph.addVertex(vertex)
            graph.addEdge(vertices_list(i-1), vertex, new LabeledEdge(vertices_list(i-1), vertex, ""))

            if (loops_info(i).nonEmpty)
                loops_info(i) foreach {elem => graph.addEdge(vertex, 
                                                             vertices_list(elem), 
                                                             new LabeledEdge(vertex, vertices_list(elem), ""))}
            
            vid += 1
        }
        
        vid
    }


    /**
     * Returns modified <classified> list where preserved
     * indices corresponds to the most long instances of processes.
     *
     * That means, we are excepting undone sequences, e.g.
     *  
     *     a -> b -> c   is OK but
     *
     *     a -> b        is not cause first one contains
     *                   second one as a prefix.
     */

    def cleanUndone(process_data: ListBuffer[(ListBuffer[String], ListBuffer[Set[Int]])], 
                    classified:   List[Int]): List[Int] = {

        var to_delete = List[Int]()

        for (i <- classified.indices) {

            for (j <- (i+1 to classified.size-1)) {

                val first  = process_data(classified(i))._1
                val second = process_data(classified(j))._1

                if (first.startsWith(second))
                    to_delete :+= classified(j)

                else if (second.startsWith(first))
                    to_delete :+= classified(i)
            }
        }

        classified.diff(to_delete)
    }


    // Takes info while obtaining process classes and exporting to the XML file.
    // Each class has its own file. By default, it's testmodelX.xml, where X is a
    // class id.
    def formTestCases(process_data: ListBuffer[(ListBuffer[String], ListBuffer[Set[Int]])], 
                      classified: List[Int], 
                      path: String) = {

        val writer = new PrintWriter(path)

        writer.write("<?xml version='1.0' encoding='UTF-8' ?>\n")
        writer.write("<TREE>\n")
        
        for (i <- classified.indices) { 
        
            val content = "  <TRAVERSE ID=\"" + i + "\">\n"
            writer.write(content)

            val buf = new xml.NodeBuffer
            val states = process_data(classified(i))._1
            val refs   = process_data(classified(i))._2
            
            for (j <- states.indices) {

                buf += <EVENT/>%Attribute(None, "NAME", Text(states(j)), Null)

                refs(j).foreach{ref => buf += <REF/>%Attribute(None, "TO_INDEX", Text(ref.toString), Null)}
            }

            buf.foreach{tag => 
                        val str = "    " + tag.toString + "\n"
                        writer.write(str)}
            writer.write("  </TRAVERSE>\n")
        }

        writer.write("</TREE>")
        writer.flush
        writer.close
    }






    /*
        process_data is constructed as follows:

        Every element of process_data list is a list
        itself representing each process in the system.
        First three entries of process_data(i) describe
        process as its identifiers. All another elements
        are states in which process has been fixed.
    */

    var events_list  = new ListBuffer[MnpEvent]
    var process_data = new ListBuffer[ListBuffer[String]]

    println("Extracting info from XML document...")
    XmlParser.fillEventsInfo(events_list)

    //
    //events_list = events_list.filter{_.id > 98649706}
    //

    println("Recovering actual event sequence...")
    events_list = events_list.reverse

    println("Creating processes' structure...")
    events_list.foreach {

        event =>
        val isnew = isProcessNew(event, process_data)

        // i.e. process is new => create a list for it
        if (isnew == -1) {

            process_data += ListBuffer(event.requestId, 
                                        event.processId, 
                                        event.npId, 
                                        event.eventName)
        }

        // in the case of existing process, isProcessNew()
        // returns index of a process
        else {

            fillInfo(event, process_data(isnew))
            process_data(isnew) += event.eventName
        }
    }

    println("Reducing state repeats to loops...")

    // modified_data is a ListBuffer of tuples (ListBuffer[String], ListBuffer[Set[Int]]) - 
    // each tuple for each process
    var modified_data = new ListBuffer[(ListBuffer[String], ListBuffer[Set[Int]])]

    process_data foreach { state_list => 

        var (reduced_list, reference_list) = reduceLoops(state_list.slice(3, state_list.size))
        var pair = (reduced_list, reference_list)

        modified_data += pair
    }

    println("Splitting processes into branches...")
    modified_data = reduceAheadRefs(modified_data)


    // Note that we distinguish processes with the same state sequence 
    // (even after reducing) but different references set (e.g. 
    //
    //
    //    a  b  c    is not the same as    a  b  c
    //    ^     |                          ^  |  |
    //    |_____|                          |__|  |
    //                                     |_____|
    //
    //
    // ) so for marking process as similar, there need to be matching
    // reduced_list and reference_list AT THE SAME INDEX in 
    // modified_data.

    println("Filtering similar processes...")
    modified_data = modified_data.distinct

    println("Initialising graph exporting providers...")

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


    println("Configuring dot exporting writer...")

    val dotfile = "data/sample.dot"
    val writer_dot = new PrintWriter(dotfile)
    val exporter = 
        new DOTExporter[LabeledVertex, LabeledEdge](
                    vertexId, 
                    vertexLabel,
                    edgeLabel)

    println("Forming classes of processes and constructing trees...")

    var unchecked  = modified_data.indices.toList
    var classified = List[Int]()
    var max_depth  = 0
    var classes_number = 0

    while (!unchecked.isEmpty) {

        val root_pattern: String = modified_data(unchecked(0))._1(0)
        val path = "data/testmodel" + classes_number.toString + ".xml"

        // always starting forming a new class with the 
        // first element appropriate to unchecked(0), 
        // so placing zero at the beginning of classified
        classified = List[Int](unchecked(0))
        max_depth  = modified_data(classified(0))._1.size

        for (i <- (1 to unchecked.size-1)) {

            val state_list = modified_data(unchecked(i))._1
                
            if (state_list(0) == root_pattern) {

                if (state_list.size > max_depth)
                    max_depth = state_list.size

                classified :+= unchecked(i)
            }
        }

        classes_number += 1
        unchecked = unchecked.diff(classified)
/*
        println("Unchecked, classified and classified with reduced undones:")
        unchecked.foreach(ind => print(ind + " "))
        println
        classified.foreach(ind => print(ind + " "))
        println
*/

        classified = cleanUndone(modified_data, classified)
        formTestCases(modified_data, classified, path)
/*
        classified.foreach(ind => print(ind + " "))
        println
*/

        val tree = new TreeLike(max_depth)

        tree.initWithProcess(modified_data(classified(0))._1, 
                             modified_data(classified(0))._2)

        for (i <- (1 to classified.size-1)) {
            tree.addProcess(modified_data(classified(i))._1, 
                            modified_data(classified(i))._2)
        }

        val graph = 
            new DefaultDirectedWeightedGraph[LabeledVertex, LabeledEdge](classOf[LabeledEdge])
        tree.injectTreeToGraph(graph, 0)
        exporter.exportGraph(graph, writer_dot)
    }

    println("Found " + classes_number + " classes.")
    println("Dot file exported.")
    writer_dot.flush
    writer_dot.close
}
