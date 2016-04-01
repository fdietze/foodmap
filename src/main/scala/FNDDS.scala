import scala.io._

object FNDDS {
  println("initializing food database...")
  def lines(file: String) = Source.fromURL(getClass.getResource(s"fndds/$file")).getLines
  def stripTilde(str: String) = str.drop(1).dropRight(1)

  val mainFoodDesc:Map[String,FoodDesc] = lines("MainFoodDesc.txt").map(FoodDesc.fromString).toIterable.groupBy(_.foodCode).mapValues(_.head)
  val mainFoodDescRelevant:Map[String,FoodDesc] = lines("MainFoodDesc_Relevant.txt").map(FoodDesc.fromString).toIterable.groupBy(_.foodCode).mapValues(_.head)
  val nutDesc = lines("NutDesc.txt").map(NutDesc.fromString).toIterable.groupBy(_.nutritientCode).mapValues(_.head)
  val nutVal = lines("FNDDSNutVal.txt").map(NutVal.fromString).toIterable.groupBy(_.foodCode).map{case (k,v) => k -> v.groupBy(_.nutritientCode).mapValues(_.head)}
  
  val food:Map[String,Food] = mainFoodDesc.map{case (id,foodDesc) => foodDesc.mainFoodDescription -> Food(id)}
  val foodRelevant:Map[String,Food] = mainFoodDescRelevant.map{case (id,foodDesc) => foodDesc.mainFoodDescription -> Food(id)}
  //lazy val foodNutritionData = food.values.map(_.nutritionData).toArray
  def nutritionData(food:Iterable[Food]) = food.map(_.nutritionData).toArray
  def nutritionData(food:Iterable[Food],filter:Seq[String]) = food.map(_.nutritionData(filter)).toArray

  case class Food(id:String) {
    def name = mainFoodDesc(id).mainFoodDescription
    lazy val nutrition = nutVal(id).map {
      case (nutritientCode,nutrition) =>
        val desc = nutDesc(nutritientCode)
        desc.nutritientDescription -> NutritionValue(nutrition.nutritientValue,desc.unit)
      }.withDefault(k=>NutritionValue(0.0,"g"))
    lazy val nutritionData = nutrition.values.map(_.absolute).toArray
    def nutritionData(filter:Seq[String]) = nutrition.filterKeys(filter contains).values.map(_.absolute).toArray
    def distance(that:Food) = Math.sqrt(nutrition.keys.map{name => math.pow(this.nutrition(name).absolute - that.nutrition(name).absolute,2)}.sum)
    def distance(filter:Seq[String])(that:Food) = Math.sqrt(filter.map{name => math.pow(this.nutrition(name).absolute - that.nutrition(name).absolute,2)}.sum)
    override def toString = s"Food($name)"
  }

  case class NutritionValue(value:Double, unit:String) {
    def absolute = value * (unit match {
      case "g" => 1.0
      case "mg" => 0.001
      case "mcg" => 0.000001
      case "mcg_DFE" => 0.000001
      case "mcg_RAE" => 0.000001
      case "kcal" => 1.0
      //case _ => 1.0
    })
  }


  object FoodDesc {
    def fromString(str:String) = {
      val a = str.split('^')
      FoodDesc(a(0), a(1), a(2), stripTilde(a(3)))
    }
  }
  case class FoodDesc(foodCode:String, startDate:String, endDate:String, mainFoodDescription:String)

  object NutDesc {
    def fromString(str:String) = {
      val a = str.split('^')
        NutDesc(a(0), stripTilde(a(1)), stripTilde(a(2)), stripTilde(a(3)), a(4))
    }
  }
  case class NutDesc(nutritientCode:String, nutritientDescription:String, tagName:String, unit:String, decimals:String)

  object NutVal {
    def fromString(str:String) = {
      val a = str.split('^')
        NutVal(a(0), a(1), a(2), a(3), a(4).toDouble)
    }
  }
  case class NutVal(foodCode:String, nutritientCode:String, startDate:String, endDate:String, nutritientValue:Double)

}
