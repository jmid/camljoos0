public class Field4 {

    protected int i = 42;
    protected String s;
    protected int j = this.i * 2;

    public Field4() throws Exception { System.out.print(this.j); }

    public static void main(String[] args) throws Exception { new Field4(); }

}
