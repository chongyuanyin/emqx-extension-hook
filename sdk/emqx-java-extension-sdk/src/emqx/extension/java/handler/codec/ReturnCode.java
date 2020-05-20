package emqx.extension.java.handler.codec;

public class ReturnCode implements HandlerParameter {
	
	public final String value;		//Atom
	
	public ReturnCode(String value) {
		this.value = value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ReturnCode (");
		sb.append("value=" + value);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		try {
//			String value = CodecUtil.atom2String(object);
//			return new ReturnCode(value);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid ReturnCode: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//	}

}
