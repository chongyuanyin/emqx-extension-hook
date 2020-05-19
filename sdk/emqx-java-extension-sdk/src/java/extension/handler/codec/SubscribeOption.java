package java.extension.handler.codec;

public class SubscribeOption implements HandlerParameter {
	
	public final boolean isNew;
	public final int nl;
	public final int qos;
	public final int rap;
	public final int rh;
	
	public SubscribeOption(boolean isNew, int nl, int qos, int rap, int rh) {
		this.isNew = isNew;
		this.nl = nl;
		this.qos = qos;
		this.rap = rap;
		this.rh = rh;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("SubscribeOption (");
		sb.append("isNew=" + isNew);
		sb.append(", nl=" + nl);
		sb.append(", qos=" + qos);
		sb.append(", rap=" + rap);
		sb.append(", rh=" + rh);
		sb.append(")");
		
		return sb.toString();
	}
	
//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		Boolean isNew = null;
//		int nl  = -1;
//		int qos = -1;
//		int rap = -1;
//		int rh = -1;
//		
//		try {
//			List<Tuple> contents = (List<Tuple>)object;
//			
//			for (Tuple tuple : contents) {
//				String key = CodecUtil.atom2String(tuple.get(0));
//				switch (key) {
//				case "is_new":
//					isNew = (Boolean)(tuple.get(1));
//					break;
//				case "nl":
//					nl = (Integer)(tuple.get(1));
//					break;
//				case "qos":
//					qos = (Integer)(tuple.get(1));
//					break;
//				case "rap":
//					rap = (Integer)(tuple.get(1));
//					break;
//				case "rh":
//					rh = (Integer)(tuple.get(1));
//					break;
//				}
//			}
//				
//			if (isNew == null ||
//					nl < 0 ||
//					qos < 0 ||
//					rap < 0 ||
//					rh < 0) {
//				String error = MessageFormat.format("Invalid SubscribeOption: {0}", object);
//				throw new InvalidParameterException(error);
//			}
//			return new SubscribeOption(isNew, nl, qos, rap, rh);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid SubscribeOption: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//	}

}
