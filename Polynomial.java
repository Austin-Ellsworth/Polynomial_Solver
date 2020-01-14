/* CLASS INTRODUCTION: 
   
   This class will construct a polynomial function in the form of f(x) = c0 + c1*x + c2*x2 + c3*x3 + .. to any length
   There will be two provate data members, one an array of coefficients, the second will be an integer value of the highest
   degree power.
   This class will include 10 methods.
   
   1. A default constructor that will initialize degree to 0 and a single array element set to 0.0. This represents f(x) = 0.
   2. A user defined constructor that will take a single string of numerical values, automatically determining the degree of the function.
   3, 4. Two accessors for data members (degree and coefficient).
   5. One modifier for updating the the coefficient array. This will take a string just like the constructor.
      NOTE: The last value in the string cannot be zero unless it is the only value, this will be protected against.
   6. An add polynomial instance method that returns a new polynomial. 
   7. A toString method that will output the polynomial.
   8. An evaluate(double) instance method that calculates the value of x and returns a double.
   9. A derivative instance method that returns a new polynomial.
   10. A findRoot(double) instance method that performs the Newton Raphson method using the passed double as an initial guess. 
      NOTE: This method will iterate up to 1000 times until accurate within 0.00001. Can throw IllegalStateException.
      
   Author: Austin Ellsworth
*/

public class Polynomial{

   private double [] coefficient;
   private int degree;
   
   //default constructor method that sets f(x) = 0
   public Polynomial (){
      degree = 0;
      coefficient = new double [1];
      coefficient[0] = 0.0;
   } //end default constructor
   
   public Polynomial (String coeff){ //user defined constructor
      setCoeff(coeff);               //setCoeff does all the work for this constructor
      
   } //end user-defined constructor

   public double[] getCoeff(){//returns coefficient
      return coefficient;
   } //end getCoeff
   
   public int getDegree(){ //returns degree
      return degree;
   } //end getDegree
   
   public void setCoeff(String coeff){ //sets the coefficient array based on input string
      String [] stringNumbers  = new String[coeff.length()];
      int index =0;
      degree = 0;

      for(int i = 0; i < coeff.length(); i++) {
            stringNumbers = coeff.split(" "); //splits string into array of values separated by spaces
            if(Character.isWhitespace(coeff.charAt(i))){
               degree++; //degree increments by space value
            }
         }  
      coefficient = new double [degree + 1];
       
      for(int i = 0; i < degree + 1; i++){
         coefficient[i] = Double.parseDouble(stringNumbers[i]); //parses strings to doubles
      } 
      for(int i = 0; i < coefficient.length; i++){
         if(coefficient[(coefficient.length - 1) - i] == 0){ //decreases degree in the case of trailing zeros
            degree--;
         }
         if(coefficient[(coefficient.length - 1) - i] > 0){ //finds the last value greater than 0
            i = coefficient.length;
         }
      }
   } //end setCoeff
   
   public String toString(){
      int times = 0; //helps to ensure '+' is not added at the beginning of the polynomial
      String output = "f(x) = ";
      for (int i = 0; i < coefficient.length; i++){ //assists in assigning degree value by using int 'i'
         if(coefficient[i] == 0){
            if(coefficient.length == 1){
               output += coefficient[i];
            }
         }
         else{
            if(coefficient[i] > -1 && times > 0) {
               if (i < coefficient.length) {
                  output += " + ";
               }
            } 
            if(coefficient[i] < 0 && times > 0) { //ensures no additon opperator on negative values and spacing is applied
               output += " ";
            }
            if(i > 1){
               output += coefficient[i] + "x^" + i;
            }
            if(i == 1) {
               output += coefficient[i] + "x"; 
            }
            if(i == 0) { 
               output += coefficient[i];
            }
            times++;
         }
      } 
      return output;
   } //end toString
   
   public Polynomial add (Polynomial poly){
      int looper = 0;
      double [] newNums = new double[(int) Math.max(this.coefficient.length, poly.coefficient.length)]; //calculates the larger of 2 arrays
      String numStrings = "";
      
      for(int i = 0; i < newNums.length; i++){
         if (this.coefficient.length > i) { 
                newNums[i] += this.coefficient[i];
            }
            if (poly.coefficient.length > i) {     // these 'if' statements allow for different
                newNums[i] += poly.coefficient[i]; // sized arrays to be added
            }
            numStrings += newNums[i];
            if(i != newNums.length - 1) {
               numStrings += " ";
            }
      }
      Polynomial newPoly = new Polynomial(numStrings);
      
      return newPoly;
   } //end add
   
   public double evaluate (double value){
      double result = 0;
      for(int i = 0; i < this.coefficient.length; i++){
         result += this.coefficient[i] * Math.pow(value, i); //i acts as the exponent
      }
      return result;
   } //end evaluate
   
   public Polynomial derivative(){ //finds the derivative of the given function
      double [] newNums = new double[this.coefficient.length];
      String numString = "";
      this.coefficient[0] = 0;
      
      for(int i = 0; i < this.coefficient.length; i++){
         newNums[i] = this.coefficient[i] * i;
      }
      for (int i = 1; i < this.coefficient.length; i++){
         newNums[i-1] = newNums[i]; //shifts the array since 'i' is acting as exponent
      }
      newNums[newNums.length - 1] = 0; //removes final value with highest exponent since it was assigned to 1 lower exponent
      
      for(int i = 0; i < newNums.length; i++){
         numString += newNums[i];
         if(i != newNums.length - 1){
            numString += " ";
         }
      }
      Polynomial newPoly = new Polynomial(numString);
      return newPoly;
   } //end derivative
   
   public double findRoot(double guess) throws Exception{ //finds estimate of one root using newton-rhapson method
      double value = 0;
      double difference = 0.00001; //creats the target error
      String numString = "";
      for (int i = 0; i < this.coefficient.length; i++){
         numString += 0;
         if (i != this.coefficient.length - 1) {
            numString += " ";
         }
      }
      
      Polynomial newPoly = new Polynomial(numString); // new polynomial created
      for (int i = 0; i < this.coefficient.length; i++){
         newPoly.coefficient[i] = this.coefficient[i]; //created a duplicate polynomial for easier derivative
      }
      newPoly = newPoly.derivative();
      
      int count = 0;
      boolean flag = true;
      
      while (flag){    
         value = guess - (this.evaluate(guess) / newPoly.evaluate(guess)); //newPoly is the derivative of original function
         if (guess - value < difference && value - guess < difference){ //target error
            flag = false;
         }
         guess = value;
         count++;
         if (count > 1000){
            throw new IllegalStateException(); // throws exception on > 1000 iterations
         }
      }
      
      return value;
   } //end findRoot 
} //end Polynomial