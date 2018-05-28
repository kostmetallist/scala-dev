package com.github.kostmetallist

import  scalax.chart.api._
import  io.github.facaiy.math.expression.MathExp


object GraphicsBuilder {

    def build(input: String, precisionLvl: Int): String = {

        // TODO place it in try block
        val expression = MathExp.parse(input)
        val rangeX = precisionLvl * 16
        val data   = for (i <- -rangeX to rangeX) yield (i, expression.eval(Map("x" -> i)))
        val chart  = XYLineChart(data)
        val path   = "./data/chart.png"

        chart.saveAsPNG(path)
        path
    }
}
