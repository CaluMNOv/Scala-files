

object Tests {
  /**
   * Test 1
   * Die Funktionen f und g werden benötigt
   */
  
def f(x: Array[Double]): Double = if(scala.math.abs(x(0)-0.4)>1){
    
    scala.math.abs(scala.math.atan(x(0) - 0.4) * scala.math.atan(x(1) - 2))
        
    }else {
      
      (1.0/4*(x(0)-0.4)*(x(0)-0.4) + (scala.math.Pi/4-1.0/4)) * scala.math.abs(scala.math.atan(x(1) - 2))
      
    }

def df(x: Array[Double]): Double = if(scala.math.abs(x(0)-0.4)>1){
  scala.math.signum(scala.math.atan(x(0) - 0.4)) *  scala.math.abs(scala.math.atan(x(1) - 2))* (1.0 / (scala.math.pow(x(0) - 0.4, 2) + 1))
}else{
  0.5*(x(0)-0.4)*scala.math.abs(scala.math.atan(x(1)-2))
}



def g(x: Array[Double]) = scala.math.cos(x(0)) * scala.math.sin(x(1) + 5)
def dg(x: Array[Double]): Double = scala.math.cos(x(0)) * scala.math.cos(x(1) + 5)

/*
 * Ausführung des Tests:
 */
//val myNash = NashCalculator(128,0.0000001,Array(f,g),Array(df,dg),Array(5,5),Array(-100,-100),Array(100,100),0.25)
//myNash.armijoBT

/**
   * Test 2
   * Die Funktionen f und g und h werden benötigt
   */
def linf[T: Numeric](x: Array[T])(implicit num: Numeric[T]): Double = {

    var res = 0.0
    for (i <- 0 until x.length) {
      if (scala.math.abs(num.toDouble(x(i))) > res) {
        res = scala.math.abs(num.toDouble(x(i)))
      }

    }
    res
  }

def help(x: Array[Double]): Double = {
    val xz = Array(x(0), x(2))

    if (linf(xz) < 0.25 && scala.math.abs(xz(1))>0.0001) {
      scala.math.sqrt(scala.math.abs(xz(0))) + scala.math.sqrt(scala.math.abs(xz(1)))
    } else if(linf(xz) < 0.25 && scala.math.abs(xz(1))<=0.0001) {
      scala.math.sqrt(scala.math.abs(xz(0))) + 50.0/4*scala.math.pow(10.0,12)*scala.math.pow(xz(1),4) + 0.00875
    }else{
      1
    }

  }

def dhelp(x: Array[Double]): Double = {

    val xz = Array(x(0), x(2))

    if (linf(xz) < 0.25 && scala.math.abs(xz(1))>0.0001) {
      
        0.5 * scala.math.signum(xz(1)) * scala.math.pow(scala.math.abs(xz(1)), -0.5)
        
    } else if (linf(xz) < 0.25 && scala.math.abs(xz(1))<=0.0001){
        50 * scala.math.pow(10.0,12)*scala.math.pow(xz(1),3)
    }    
    else {
      0.0
    }

  }


def h(x: Array[Double]): Double = {

    help(Array(x(0) - 0.5, x(1), x(2) - 0.5)) + help(Array(x(0) - 0.5, x(1), x(2) + 0.5)) + 
    help(Array(x(0) + 0.5, x(1), x(2) + 0.5)) + help(Array(x(0) + 0.5, x(1), x(2) - 0.5))
  }

  def dh(x: Array[Double]): Double = {
    dhelp(Array(x(0) - 0.5, x(1), x(2) - 0.5)) + dhelp(Array(x(0) - 0.5, x(1), x(2) + 0.5)) + 
    dhelp(Array(x(0) + 0.5, x(1), x(2) + 0.5)) + dhelp(Array(x(0) + 0.5, x(1), x(2) - 0.5))
  }
  
  
/*
 * Ausführung des Tests
 */
// val myNash = NashCalculator(128,0.0000001,Array(f,g,h),Array(df,dg,dh),Array(5,5,0.4),Array(-100,-100,-100),Array(100,100,100),0.25)
// myNash.armijoBT
  
}