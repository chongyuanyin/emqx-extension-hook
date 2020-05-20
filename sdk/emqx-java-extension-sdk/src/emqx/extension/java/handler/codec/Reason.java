package emqx.extension.java.handler.codec;

public class Reason implements HandlerParameter {
	
	public final String value;		//Binary
	
	public Reason(String value) {
		this.value = value;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Reason (");
		sb.append("value=" + value);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		try {
//			String value = CodecUtil.binary2String(object);
//			return new Reason(value);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid Reason: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//	}

}
