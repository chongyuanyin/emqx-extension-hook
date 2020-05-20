package emqx.extension.java.handler.codec;

public class Topic implements HandlerParameter {
	
	public final String value;		//Binary
	
	public Topic(String value) {
		this.value = value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Topic (");
		sb.append("value=" + value);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		try {
//			String value = CodecUtil.binary2String(object);
//			return new Topic(value);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid Topic: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//	}

}
