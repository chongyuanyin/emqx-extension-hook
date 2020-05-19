package java.extension.handler.codec;

public class ConnInfo implements HandlerParameter {

	public final String node;	//Atom
	public final String clientId;	//Binary
	public final String userName;	//Binary
	public final String peerHost;	//Binary
	public final int sockPort;	//int
	public final String protoName;	//Binary
	public final int protoVersion;	//int
	public final int keepalive;		//int
	
	public ConnInfo(String node, String clientId, String userName, String peerHost,
			int sockPort, String protoName, int protoVersion, int keepalive) {
		this.node = node;
		this.clientId = clientId;
		this.userName = userName;
		this.peerHost = peerHost;
		this.sockPort = sockPort;
		this.protoName = protoName;
		this.protoVersion = protoVersion;
		this.keepalive = keepalive;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ConnInfo (");
		sb.append("node=" + node);
		sb.append(", clientId=" + clientId);
		sb.append(", userName=" + userName);
		sb.append(", peerHost=" + peerHost);
		sb.append(", sockPort=" + sockPort);
		sb.append(", protoName=" + protoName);
		sb.append(", protoVersion=" + protoVersion);
		sb.append(", keepalive=" + keepalive);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		String node = null;	//Atom
//		String clientId = null;	//Binary
//		String userName = null;	//Binary
//		String peerHost = null;	//Binary
//		int sockPort = -1;	//int
//		String protoName = null;	//Binary
//		int protoVersion = -1;	//int
//		int keepalive = -1;	//int
//		
//		try {
//			List<Tuple> contents = (List<Tuple>)object;
//			
//			for (Tuple tuple : contents) {
//				String key = CodecUtil.atom2String(tuple.get(0));
//				switch (key) {
//				case "node":
//					node = CodecUtil.atom2String(tuple.get(1));
//					break;
//				case "clientid":
//					clientId = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "username":
//					userName = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "peerhost":
//					peerHost = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "sockport":
//					sockPort = (Integer) tuple.get(1);
//					break;
//				case "proto_name":
//					protoName = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "proto_ver":
//					protoVersion = (Integer) tuple.get(1);
//					break;
//				case "keepalive":
//					keepalive = (Integer) tuple.get(1);
//					break;
//				}
//			}
//			
//			if (node == null ||
//					clientId == null ||
//					userName == null ||
//					peerHost == null ||
//					sockPort < 0 ||
//					protoName == null ||
//					protoVersion < 0 ||
//					keepalive < 0) {
//				String error = MessageFormat.format("Invalid ConnInfo: {0}", object);
//				throw new InvalidParameterException(error);
//			}
//			
//			return new ConnInfo(node, clientId, userName, peerHost, sockPort, protoName, protoVersion, keepalive);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid ConnInfo: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//	}
}
