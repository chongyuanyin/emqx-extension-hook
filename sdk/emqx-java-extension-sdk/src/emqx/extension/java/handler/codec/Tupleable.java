package emqx.extension.java.handler.codec;

import erlport.terms.Tuple;

public interface Tupleable {
	
	public Tuple toTuple();

}
