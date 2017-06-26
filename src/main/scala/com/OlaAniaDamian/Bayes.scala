package com.OlaAniaDamian

import org.apache.spark.sql.{Dataset, SparkSession}
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Shape
import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JLabel
import org.jfree.util.ShapeUtilities
import org.scalatest.selenium.WebBrowser.Dimension
import scala.collection.mutable.MutableList
import scala.io.Source
import scala.util.parsing.json.JSON._
import scala.math._
import scalax.chart.api._

class Bayes {

  val sparkSession = SparkSession.builder.
    master("local")
    .appName("spark session example")
    .getOrCreate()


  def bayesData(): Unit = {
    
    val source: String = Source.fromFile("./src/main/resources/dane.json").getLines.mkString
    val source2 = source.replaceAll("\t", "")
    val source3 = source2.replaceAll("\"", "")
    val source4 = source3.replaceAll(" ", "")
    val source5 = source4.replaceAll("\\[", "")
    val source6 = source5.split(":")(2)
    val source7 = source6.replaceAll("\\]","")
    val source8 = source7.replaceAll("\\{","")
    val source9 = source8.replaceAll("\\}","")
    val data = source9.split(",")
    val x1 = MutableList[Int]()
    val y1 = MutableList[Int]()
    val x2 = MutableList[Int]()
    val y2 = MutableList[Int]()
    for(i <- 0 until data.length-3 by 4){
      x1 += data(i).toInt
      y1 += data(i+1).toInt
      x2 += data(i+2).toInt
      y2 += data(i+3).toInt
    }
    val newX = 26
    val newY = 25
    val r = 10
    val closestRed = closestPoints(x1, y1, newX, newY, r)
    val closestGreen = closestPoints(x2, y2, newX, newY, r)
    val (newPointGreen, newPointRed) = Bayes(closestGreen.length, closestRed.length, x1.length, x2.length)
    if(newPointRed) {
        x1+=newX
        y1+=newY
    }
    else {
      x2+=newX
      y2+=newY
    }    
    val series = new XYSeries("Class 1")
        for ((a,b) <- x1 zip y1) {
          series.add(a,b)
    }
    val series2 = new XYSeries("Class 2")
        for ((a,b) <- x2 zip y2) {
          series2.add(a,b)
    }
    val newPointSeries = new XYSeries("New Point")
    newPointSeries.add(newX,newY)
    val seriesCircle = new XYSeries("area")
    for (x <- newX.toDouble-r to newX.toDouble+r by 0.01) {
        seriesCircle.add(x,sqrt(r*r-(x-newX)*(x-newX))+newY)
        seriesCircle.add(x,-sqrt(r*r-(x-newX)*(x-newX))+newY)
    } 
    
    val nP = new Ellipse2D.Double(-5, -5, 10, 10)
    val SeriesColl = new XYSeriesCollection()
    SeriesColl.addSeries(series)
    SeriesColl.addSeries(series2)
    SeriesColl.addSeries(newPointSeries)
    SeriesColl.addSeries(seriesCircle)
    val chart1 = XYLineChart(SeriesColl)
    val yAxis = chart1.plot.range.axis.peer.asInstanceOf[org.jfree.chart.axis.NumberAxis]
    yAxis.setLowerBound(8)
    yAxis.setUpperBound(42)
    val xAxis = chart1.plot.domain.axis.peer.asInstanceOf[org.jfree.chart.axis.NumberAxis]
    xAxis.setLowerBound(8)
    xAxis.setUpperBound(42)
    chart1.plot.setRenderer(new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer(false, true))
    chart1.plot.getRenderer().setSeriesPaint(1, new Color(0x00, 0xFF, 0x00))
    chart1.plot.getRenderer().setSeriesPaint(0, new Color(0xFF, 0x00, 0x00))
    chart1.plot.getRenderer().setSeriesPaint(2, new Color(0xFF, 0xFF, 0x00))
    chart1.plot.getRenderer().setSeriesPaint(3, new Color(0x00, 0x00, 0xFF))
    chart1.plot.getRenderer().setSeriesShape(0,new Ellipse2D.Double(-2.5d, -2.5d, 5d, 5d))
    chart1.plot.getRenderer().setSeriesShape(1,new Ellipse2D.Double(-2.5d, -2.5d, 5d, 5d))
    chart1.plot.getRenderer().setSeriesShape(2,nP)
    chart1.plot.getRenderer().setSeriesShape(3,new Ellipse2D.Double(-2.5d, -2.5d, 1d, 1d))
    chart1.plot.setBackgroundPaint(new Color(0xFF, 0xFF, 0xFF))
    chart1.show()
    Thread.sleep(500000)
  }
  
  def closestPoints(x: MutableList[Int], y: MutableList[Int], newX: Int, newY: Int, r: Int): MutableList[Int] ={
    var x_distance = 0
    var y_distance = 0
    var distance = 0.0
    var closestList = new MutableList[Int]()
    for ((i, j) <- (x zip y)){
      x_distance = i - newX
      y_distance = j - newY
      distance = sqrt(pow(x_distance, 2) + pow(y_distance, 2))
      if (distance < r)
      {
        closestList += i
      }
    }
    
    return closestList
  }
  
  def Bayes(closestGreen: Int, closestRed: Int, amountRed: Int, amountGreen: Int): (Boolean, Boolean) = {
    var newPointGreen = false
    var newPointRed = false
    val allPoints = amountRed + amountGreen
    val aprioriGreen = amountGreen.toDouble / allPoints.toDouble
    val aprioriRed = amountRed.toDouble / allPoints.toDouble
    val probGreen = closestGreen.toDouble / amountGreen.toDouble
    val probRed = closestRed.toDouble / amountRed.toDouble
    val aposterioriGreen = aprioriGreen * probGreen
    val aposterioriRed = aprioriRed * probRed
    if (aposterioriGreen > aposterioriRed)
      newPointGreen = true
    else
      newPointRed = true
  
    return (newPointGreen, newPointRed)
    
  }
}