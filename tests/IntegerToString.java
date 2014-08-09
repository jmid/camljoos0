public class IntegerToString {
    public IntegerToString() throws Exception {
	this.printNumber(this.parseNumber(), 8);
    }
    
    public static void main(String[] args) throws Exception {
	new IntegerToString();
    }

    public int parseNumber() throws Exception {
	int c = 0;
	int number = 0;
	System.out.print("Enter a number: ");
	while ((c = System.in.read()) >= 48)
	    number = 10*number + (c-48);
	return number;
    }
    
    public void printNumber(int number, int base) throws Exception {
	String ns = "";
	System.out.print("In base "+base+" that is: ");
	if (number == 0) {
	    ns = "0";
	} else {
	    while (number > 0) {
		ns = (number % base) + ns;
		number = number / base;
	    }
	}
	System.out.print(ns);
	return;
    }
}
