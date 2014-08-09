public class Cons {
    protected int first;
    protected Cons rest;

    public Cons(int first, Cons rest) throws Exception { 
	this.first = first;
	this.rest = rest;
    }

    public boolean member(int item) throws Exception { 
	boolean result = false;
	if (this.first == item)
	    result = true;
	else if (this.rest == null)
	    result = false;
	else
	    result = this.rest.member(item);
	return result;
    }
}
