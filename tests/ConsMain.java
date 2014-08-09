public class ConsMain {

    public ConsMain() throws Exception {
	Cons mylist = new Cons(1,new Cons(2,new Cons(3,new Cons(4,null))));
	System.out.print(mylist.member(3));
	System.out.print(mylist.member(5));
    }

    public static void main(String[] args) throws Exception {
	new ConsMain();
    }
}
