import org.encog.Encog
import org.encog.ml.data.MLData
import org.encog.ml.data.MLDataSet
import org.encog.ml.data.basic.BasicMLData
import org.encog.ml.data.basic.BasicMLDataSet
import org.encog.neural.som.SOM
import org.encog.neural.som.training.basic.BasicTrainSOM
import org.encog.neural.som.training.basic.neighborhood._
import org.encog.mathutil.rbf.RBFEnum._

import swing._
import event._
import java.awt.Color
import java.awt.Dimension
import java.awt.Point
import java.awt.Rectangle
import java.awt.event._
import javax.swing._

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

case class Sample(point:Point, color:Color, name:String)

object Main extends SimpleSwingApplication {

  var plotData:Map[Int,Iterable[Sample]] = Map.empty
  val optimalsize = 30 //math.ceil(math.sqrt(FNDDS.foodRelevant.size)*2).toInt
  val mapSize = new Dimension(optimalsize,optimalsize)
  val scale = 400/optimalsize

  val textBox = new Label(""){
    preferredSize = new Dimension(600,mapSize.height*scale)
  }

  val map = new Panel {
    background = new Color(238,238,238)
    preferredSize = new Dimension(scale*mapSize.width,scale*mapSize.height)
    maximumSize = preferredSize
    minimumSize = preferredSize

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      for((_,samples) <- plotData){
        val red = samples.map(_.color.getRed).sum / samples.size
        val green = samples.map(_.color.getGreen).sum / samples.size
        val blue = samples.map(_.color.getBlue).sum / samples.size
        g.setColor(new Color(red,green,blue))
        val point = samples.head.point
        val rect = new Rectangle(point.x*scale,point.y*scale,scale,scale)
        g.fill(rect)
      }
    }

    listenTo(mouse.moves)

    reactions += {
      case MouseMoved(_,point,_) => textBox.text = {
        val outNeuron = (point.x / scale) + (point.y/scale)*mapSize.width
        "<html>"+
        plotData.withDefault(k => Sample(null,null,"") :: Nil)(outNeuron).map(_.name + "<br>").mkString +
        "</html>"
      }
    }
  }

  val top = new MainFrame {
    contents = new BoxPanel(Orientation.Horizontal){
      contents += map
      contents += new BoxPanel(Orientation.Vertical){
        // contents += new ActionButton("print", {
        //   draw(plotData.values.flatten)
        // })
        contents += textBox
      }
    }

    override def closeOperation = {
      sys.exit()
    }
  }

  def draw(data:Iterable[Sample]){
    val size = 400

      val bi = new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB)
      val squaresize = size/optimalsize.toDouble

      val ig2 = bi.createGraphics()
      ig2.setPaint ( Color.white )
      ig2.fillRect ( 0, 0, bi.getWidth(), bi.getHeight() )
      val font = new Font("Sans", Font.BOLD, 20)
      ig2.setFont(font)
    println(data.zipWithIndex.map{case (food,id) => s"$id: ${food.name}"}.mkString("\n"))

      for((Sample(point,color,name),id) <- data.zipWithIndex){
        val fontMetrics = ig2.getFontMetrics();
        val stringWidth = fontMetrics.stringWidth(name);
        val stringHeight = fontMetrics.getAscent();
        ig2.setPaint(Color.black)
        val p = new Point((point.x*squaresize).toInt,(point.y*squaresize).toInt)
        ig2.drawString(id.toString, p.x, p.y)
      }

      ImageIO.write(bi, "PNG", new File("map.png"));
  }

  var update = false
  val timer = new Timer(200, new AbstractAction() {
      def actionPerformed(e: java.awt.event.ActionEvent) { if(update) top.repaint(); update = false }
    })
  timer.start()


  Future {

    val banana = FNDDS.food("Banana, raw")
    val apple = FNDDS.food("Apple, raw")
    //println(banana)
    //println(banana.nutrition.filterKeys(interesting contains))
    //println(apple)
    //println(apple.nutrition.filterKeys(interesting contains))

    //println("distance(apple,banana): %s" format(banana.distance(interesting)(apple)))

    //println("closest foods to banana:")
    //println(FNDDS.food.values.toList.sortBy(banana.distance).take(20).map(f => (f,banana.distance(interesting)(f))).mkString("\n"))

    val interesting:Seq[String] = List("Alcohol", "Water"/*,"Total Fat"*/,"Fatty acids, total monounsaturated","Fatty acids, total polyunsaturated","Fatty acids, total saturated","Protein","Carbohydrate","Sugars, total","Fiber, total dietary")
    println("classifying over: " + interesting.mkString(" | "))

    println("preparing training data...")
    val food = FNDDS.food
    val data = FNDDS.nutritionData(food.values,interesting)

    //println(data.take(10).map(_.mkString(", ")).mkString("\n"))

    val trainingData = new BasicMLDataSet(data,null)

    println("creating neuronal network...")
    val network = new SOM(data.head.size,mapSize.width*mapSize.height)
    network.reset() // TODO: make deterministic
    println("input neurons:  " + network.getInputCount)
    println("output neurons: " + network.getOutputCount)

    val train = new BasicTrainSOM(network, 0.5, trainingData, new NeighborhoodRBF(Gaussian,mapSize.width,mapSize.height))
    train.setForceWinner(false)

    def coords(i:Int) = new Point(i % mapSize.width, i / mapSize.width)
    def transform(f:FNDDS.Food) = {
      val out = network.classify(new BasicMLData(f.nutritionData(interesting)))
      out -> Sample(coords(out),
        f.id.take(1) match {
          case "1" => new Color(155,228,255)//(1) milk and milk products
          case "2" => new Color(255,155,167)//(2) meat, poultry, fish, and mixtu
          case "3" => new Color(255,244,155)//(3) eggs
          case "4" => new Color(214,171,117)//(4) legumes, nuts, and seeds
          case "5" => new Color(255,220,155)//(5) grain products
          case "6" => new Color(251,180,255)//(6) fruits
          case "7" => new Color(159,227,133)//(7) vegetables
          case "8" => new Color(133,204,227)//(8) fats, oils, and salad dressing
          case "9" => new Color(202,195,255)//(9) sugars, sweets, and beverages
          // case "1" => new Color(0,154,254)//(1) milk and milk products
          // case "2" => new Color(254,125,97)//(2) meat, poultry, fish, and mixtu
          // case "3" => new Color(0,189,204)//(3) eggs
          // case "4" => new Color(181,190,70)//(4) legumes, nuts, and seeds
          // case "5" => new Color(198,135,36)//(5) grain products
          // case "6" => new Color(247,105,169)//(6) fruits
          // case "7" => new Color(71,131,28)//(7) vegetables
          // case "8" => new Color(0,200,136)//(8) fats, oils, and salad dressing
          // case "9" => new Color(175,148,254)//(9) sugars, sweets, and beverages
        }
      , f.name)
    }

    def exportFrame(i:Int, data: Map[Int,Iterable[Sample]]) {
      val size = optimalsize
      val bi = new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB)

      val ig2 = bi.createGraphics()
      ig2.setPaint ( Color.white )
      ig2.fillRect ( 0, 0, bi.getWidth(), bi.getHeight() )

      for((_,samples) <- plotData){
        val red = samples.map(_.color.getRed).sum / samples.size
        val green = samples.map(_.color.getGreen).sum / samples.size
        val blue = samples.map(_.color.getBlue).sum / samples.size
        ig2.setPaint(new Color(red,green,blue))
        val point = samples.head.point
        val rect = new Rectangle(point.x,point.y,1,1)
        ig2.fill(rect)
      }

      ImageIO.write(bi, "PNG", new File("frame_%04d.png" format i));
    }


    plotData = food.values.map(transform).groupBy(_._1).mapValues(_.map(_._2))
    update = true

    println("training network...")
    for(i <- 0 until 200) {
      train.iteration()
      println("%4d: training error: %8.6f, learning rate: %8.6f" format(i, train.getError, train.getLearningRate))
      plotData = food.values.map(transform).groupBy(_._1).mapValues(_.map(_._2))
      update = true
      train.decay(0.02)
      // exportFrame(i, plotData)
    }


    //println("apple: "+ network.classify(new BasicMLData(apple.nutritionData(interesting))))
    //println("banana: "+ network.classify(new BasicMLData(banana.nutritionData(interesting))))
  }
}

class ActionButton(title:String, action: => Unit) extends Button(title) {
  reactions += {
    case e:ButtonClicked => action
  }
}
