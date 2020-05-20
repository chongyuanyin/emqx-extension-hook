package emqx.extension.java.handler;

import erlport.terms.Tuple;

public interface CommunicationHandler {
	
	public Object init();
	
	public void deinit();

}
