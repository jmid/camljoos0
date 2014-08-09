public class D {

    protected int f;
    protected String sf = "hello, world!";

    public D() throws Exception { System.out.print(this.sf); }

    public static void main(String[] args) throws Exception { new D(); }

    public void m1(int a, String s) throws Exception { return; }
    public String m2() throws Exception { return "" + System.in.read(); }
}
