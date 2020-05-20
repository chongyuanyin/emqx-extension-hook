package emqx.extension.java.handler.codec;

public class Property implements HandlerParameter {
	
	public final String key;		//Atom
	public final String value;	//Binary
	
	public Property(String key, String value) {
		this.key = key;
		this.value = value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Property (");
		sb.append("key=" + key);
		sb.append(", value=" + value);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		String key = null;
//		String value = null;
//		
//		try {
//			Tuple tuple = (Tuple)object;
//			key = CodecUtil.atom2String(tuple.get(0));
//			value = CodecUtil.binary2String(tuple.get(1));
//			
//			if (key == null ||
//					value == null) {
//				String error = MessageFormat.format("Invalid Property: {0}", object);
//				throw new InvalidParameterException(error);
//			}
//			
//			return new Property(key, value);
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid Property: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//		
//	}

}
