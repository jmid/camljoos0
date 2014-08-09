public class FacRec {

    public FacRec() throws Exception {
	System.out.print(this.fac(5));
    }

    public static void main(String[] args) throws Exception {
	new FacRec();
    }

    public int fac(int i) throws Exception {
	int result = 0;

	if (i==0) {
	    result = 1;
	}
	else { 
	    result = i * this.fac(i-1);
	}
	return result;
    }
}
