
/**
 * Löst die Komplementaritätsbedingungen zwischen Preis und erzeugter Strommenge
 */
object Renewables {

  /*
   * Vorgegebene Variablen
   */
  val demand = 550000000D // 550 TWh jährlich angegeben in MWh
  val shedcost = 10000000D // 1 Million Euro je MWh

  /**
   * Lastabwurffunktion
   * @param x die Stromerzeugung durch die verschiedenen Erzeuger als Array
   * @return Bedarf - Gesamterzeugung + statistische Größe (1 Millonstel der Erzeugung)
   */
  def shed(x: Array[Double]): Double = {
    var ret: Double = 0
    var entire = 0.0
    for (i <- 0 until x.length) {
      entire += x(i)
    }

    var chance = 0.0
    if (entire == 0) chance = 0 else {

      val approx = Array(0, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000002, 0.000005, 0.000008, 0.000015, 0.00006, 0.00012, 0.0005, 0.0009, 0.00025, 0.0001,
        0.0001, 0.001, 0.005, 0.01)

      chance = if ((x(0) + x(1)) / entire <= 0.3) entire * 0.000001 else {
        entire * 10 * (approx(((x(0) + x(1)) / entire * 20).toInt) + ((x(0) + x(1)) / entire * 20 - ((x(0) + x(1)) / entire * 20).toInt) * approx(((x(0) + x(1)) / entire * 20).toInt + 1))
      }
    }
    ret = scala.math.max(0, demand - entire + chance)

    ret
  }

  /**
   * Ableitung der Lastabwurffunktion
   * @param x Die aktuelle Erzeugung
   * @param i Die Position die abgeleitet werden soll
   * @return Die Veränderung des Lastabwurfs bei zusätzlicher Erzeugung von 1 MWh mit der i-ten Technologie
   */
  def dshed(x: Array[Double], i: Int): Double = {
    var entire = 1.0
    for (i <- 0 until x.length) {
      entire += x(i)
    }

    val xnew = x.clone
    xnew(i) += 1

    val approx = Array(0, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000001, 0.000002, 0.000005, 0.000008, 0.000015, 0.00006, 0.00012, 0.0005, 0.0009, 0.00025, 0.0001,
      0.0001, 0.001, 0.005, 0.01)

    var ret = shed(xnew) - shed(x)

    ret
  }

  /* 
   * Array der Menge der Stromerzeugung
   * Angaben in 1 MWh tatsächlicher durchschnittlicher Erzeugung
   * x_k
   * 0 - Wind
   * 1 - Solar
   * 2 - Biomasse
   * 3 - Wasser
   * 4 - Gas 
   * 5 - Kohle
   * 6 - Atom
   */
  val techs: Array[Double] = new Array[Double](7)

  /*
   * Installationskosten C_k
   * Umgerechnet in tatsächliche Leistung
   * 0 - 900.000 für 1 MW Wind (installierte Leistung) => 3.600.000
   * 1 - 1.500.000 für 1 MW Solar (installierte Leistung) => 6.000.000
   * 2 - 2.500.000 für 1 MW Biogas (tatsächlich abrufbare Leistung)
   * 3 - 6.250.000 für 1 MW Wasser
   * 4 - 650.000 für 1 MW Gas
   * 5 - 1.500.000 für 1 MW Kohle
   * 6 - 3.000.000 für 1 MW Atom
   */
  val costs: Array[Double] = Array(3600000D, 6000000, 2500000, 6250000, 650000, 1500000, 3000000)

  /*
   * Laufende Kosten g_k
   * 0 - 15
   * 1 - 10
   * 2 - 60
   * 3 - 40 für 1 MW
   * 4 - 70
   * 5 - 40
   * 6 - 22
   */
  val rcosts: Array[Double] = Array(15, 10, 60, 50, 70, 40, 22)
  for (i <- 0 until rcosts.length) {
    rcosts(i) = 8600 * rcosts(i)
  }

  def cost(x: Array[Double]): Double = {
    var ret = 0.0

    for (i <- 0 until techs.length) {
      ret += costs(i) * x(i) // Installationskosten 
      ret += rcosts(i) * x(i) // Laufende Kosten
    }

    ret += shed(x) * shedcost // Kosten für voraussichtliche Lastabwürfe
    ret
  }

  def wind(x: Array[Double]): Double = { cost(x) }
  def solar(x: Array[Double]): Double = { cost(x) }
  def biogas(x: Array[Double]): Double = { cost(x) }
  def water(x: Array[Double]): Double = { cost(x) }
  def gas(x: Array[Double]): Double = { cost(x) }
  def carbon(x: Array[Double]): Double = { cost(x) }
  def atom(x: Array[Double]): Double = { cost(x) }

  def dwind(x: Array[Double]): Double = costs(0) + rcosts(0) + dshed(x, 0) * shedcost
  def dsolar(x: Array[Double]): Double = costs(1) + rcosts(1) + dshed(x, 1) * shedcost
  def dbiogas(x: Array[Double]): Double = costs(2) + rcosts(2) + dshed(x, 2) * shedcost
  def dwater(x: Array[Double]): Double = costs(3) + rcosts(3) + dshed(x, 3) * shedcost
  def dgas(x: Array[Double]): Double = costs(4) + rcosts(4) + dshed(x, 4) * shedcost
  def dcarbon(x: Array[Double]): Double = costs(5) + rcosts(5) + dshed(x, 5) * shedcost
  def datom(x: Array[Double]): Double = costs(6) + rcosts(6) + dshed(x, 6) * shedcost 
  
  def diff(x:Array[Double],i:Int) : Double = {
    var help :Array[Array[Double] => Double]= Array(dwind, dsolar, dbiogas, dwater, dgas, dcarbon, datom)
    
    help(i)(x)        
  }
  
  /*
   * CO2-Emissionen in g je MWh
   * 0 - 15000 für 1 MW Wind
   * 1 - 15000 für 1 MW Solar
   * 2 - 600000 für 1 MW Biogas
   * 3 - 30000 für 1 MW Wasser
   * 4 - 400000 für 1 MW Gas
   * 5 - 800000 für 1 MW Kohle
   * 6 - 45000 für 1 MW Atom
   * 
   * Im Schnitt bisher 474g pro kWh -> 0,474 t pro MWh
   * Angaben im Array in Tonnen
   */
  val emissions: Array[Double] = Array(0.015D, 0.015, 0.4, 0.03, 0.6, 0.8, 0.045)

  def emitted(pow: Array[Double]): Double = {

    var ret: Double = 0;

    for (i <- 0 until pow.length)
      ret += emissions(i) * pow(i)

    ret

  }

  def getMin(pow: Array[Double]): Int = {
    var ret = 0;

    for (i <- 1 until pow.length) {
      if(emissions(i) < emissions(ret))
        ret = i;
    }

    ret
  }
  
  def getMax(pow: Array[Double]): Int = {
    var ret = 0;

    for (i <- 1 until pow.length) {
      if(pow(i) > 100000 && emissions(i) > emissions(ret))
        ret = i;
    }

    ret
  }

  // 176 Millionen Tonnen im Stromsektor bisher
  // Ziel: Setze beliebigen Wert
  val emlimit: Double = 176D

  /**
   * Berechnet den kostengünstigsten Weg, um CO2 einzusparen
   */
  def savetech(pow: Array[Double], limit: Double): Array[Double] = {
    var ret = pow.clone

    /*
     * max: Index des Stromtyps, mit dem am günstigsten CO2 eingespart werden kann, indem weniger produziert wird
     * min: Index des Stromtyps, mit dem am leichtesten CO2-armer Strom produziert werden kann
     */
    var min, max = 0
    
    var end = 100

    
    
    while (emitted(ret) > limit && end > 0) {
      min = getMin(ret)
      max = getMax(ret)
      
      ret(min) += 100000
      ret(max) -= 100000
      end -= 1
    }

    
    ret
  }
  
  def bestSolution(pow:Array[Double]) : Pair[Int,Int] = {
    var i = 0
    var j = 6
    
    for(k <- 0 until pow.length;
        l <- 0 until pow.length){
      
      if(emissions(k) < emissions(l) && pow(l) > 100000){
        if((diff(pow,k) - diff(pow,l))/scala.math.abs(emissions(k) - emissions(l)) < (diff(pow,i) - diff(pow,j))/scala.math.abs(emissions(i) - emissions(j))){
          i = k
          j = l
          
        }
      }
      
    }   
    Pair(i,j)
  }
  
  def modify(pow : Array[Double], low : Int, high : Int) : Array[Double] = {
    var ret = pow
    var end = 100
    
    while(emitted(ret) > emlimit && end > 0 && bestSolution(ret) == (low,high)){
      ret(low) += 100000
      ret(high)-= 100000
    }
    
    ret
  }
  
  

  def main() = {
    var myNash = NashCalculator(128, 0.000001, Array(wind, solar, biogas, water, gas, carbon, atom), Array(dwind, dsolar, dbiogas, dwater, dgas, dcarbon, datom),
      Array(10D, 10, 10, 10, 10, 10, 10),
      Array(0D, 0, 0, 0, 0, 0, 0), Array(1000000000D, 1000000000D, 1000000000D, 1000000000D, 1000000000D, 1000000000D, 1000000000D), 0.25)
    var ret = myNash.armijoBT

    println("Erste Berechnung")
    for (i <- 0 to 6) {
      println(ret(i))
    }
    
    for( i<- 0 to 10){
      
      var(min,max) = bestSolution(ret)
      
      ret = modify(ret,min,max)
      
      var limitsmin : Array[Double] =  Array(0D, 0D, 0D, 0D, 0D, 0D, 0D)
      var limitsmax : Array[Double] =  Array(1000000000D, 1000000000D, 1000000000D, 1000000000D, 1000000000D, 1000000000D, 1000000000D)
      
      limitsmin(min) = ret(min) 
      limitsmax(max) = ret(max) 
      
      myNash = NashCalculator(128, 0.000001, Array(wind, solar, biogas, water, gas, carbon, atom), Array(dwind, dsolar, dbiogas, dwater, dgas, dcarbon, datom),
      ret,limitsmin, limitsmax, 0.25)
      
      ret = myNash.armijoBT
      
      println("Modifizierung " + (i + 1) +":")      
      for (i <- 0 to 6) {
        println(ret(i))
      }
      
      
    }
    
    
  }

}