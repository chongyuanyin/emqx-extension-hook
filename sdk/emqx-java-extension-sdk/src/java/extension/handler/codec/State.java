package java.extension.handler.codec;

import java.extension.exceptions.InvalidParameterException;
import java.io.Serializable;

public class State implements Serializable {

	private static final long serialVersionUID = -8994341646371322513L;
	
	Integer times;

    public State() {
        times = 0;
    }

    public Integer incr() {
        times += 1;
        return times;
    }

    @Override
    public String toString() {
        return String.format("State(times: %d)", times);
    }

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		return (State) object;
//	}
}
