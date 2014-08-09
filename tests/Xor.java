public class Xor {

    public Xor() throws Exception { 
	System.out.print (true ^ true);
	System.out.print (true ^ false);
	System.out.print (false ^ true);
	System.out.print (false ^ false);

	System.out.print (" ");

	System.out.print (true != true);
	System.out.print (true != false);
	System.out.print (false != true);
	System.out.print (false != false);

	System.out.print (" ");
    }

    public static void main(String[] args) throws Exception {
	new Xor();
    }
}
