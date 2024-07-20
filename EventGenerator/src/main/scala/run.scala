package ru.ispras.dst


object Executer extends App {

    val set = scala.collection.mutable.Set(1, 2, 3, 4, 5, 6)
    
    Generator.makeChains(5, set, "data/dotfile.dot")
}
