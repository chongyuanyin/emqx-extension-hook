package emqx.extension.java.handler.codec;

public class PubSub implements HandlerParameter {
	
	public final String value;		//Atom
	
	public PubSub(String value) {
		this.value = value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("PubSub (");
		sb.append("value=" + value);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		try {
//			String value = CodecUtil.atom2String(object);
//			return new PubSub(value);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid PubSub: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//	}

}
