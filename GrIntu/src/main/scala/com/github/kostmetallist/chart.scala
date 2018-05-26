package com.github.kostmetallist

import  scalax.chart.api._
import  io.github.facaiy.math.expression.MathExp
import  scala.io.StdIn

import  scala.math._


object MyChartApp extends App with scalax.chart.module.Charting {


    val str = readLine()

    //val str = "1 + 5 / 2"
	val ex = MathExp.parse(str)


	//val variables = Map("x" -> 2)
	//val output = ex.eval(variables)
	//val output = ex.eval(Map[String, Double]())

	//println("Evaluated as " + output)


    val data = for (i <- -16 to 16) yield (i, ex.eval(Map("x" -> i)))

    val chart = XYLineChart(data)
    chart.saveAsPNG("./data/chart.png")
}
