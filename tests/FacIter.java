public class FacIter {

    public FacIter() throws Exception {
	System.out.print(this.fac(5));
    }

    public static void main(String[] args) throws Exception {
	new FacIter();
    }

    public int fac(int i) throws Exception {
	int result = 1; /* Ooops */

	while (i > 0) {
	    result = result * i;
	    i = i - 1;
	}
	return result;
    }
}
