package java.extension.handler.codec;

public class ClientInfo implements HandlerParameter {
	
	public final String node;		//Atom
	public final String clientId;	//Binary
	public final String userName;		//Binary
	public final String password;		//Binary
	public final String peerHost;		//Binary
	public final int sockPort;		//int
	public final String protocol;		//Atom
	public final String mountPoint;	//Binary
	public final boolean isSuperUser;	//boolean
	public final boolean anonymous;	//boolean
	
	public ClientInfo(String node, String clientId, String userName, String password, String peerHost,
			int sockPort, String protocol, String mountPoint, boolean isSuperUser, boolean anonymous) {
		this.node = node;
		this.clientId = clientId;
		this.userName = userName;
		this.password = password;
		this.peerHost = peerHost;
		this.sockPort = sockPort;
		this.protocol = protocol;
		this.mountPoint = mountPoint;
		this.isSuperUser = isSuperUser;
		this.anonymous = anonymous;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ClientInfo (");
		sb.append("node=" + node);
		sb.append(", clientId=" + clientId);
		sb.append(", userName=" + userName);
		sb.append(", password=" + password);
		sb.append(", peerHost=" + peerHost);
		sb.append(", sockPort=" + sockPort);
		sb.append(", protocol=" + protocol);
		sb.append(", mountPoint=" + mountPoint);
		sb.append(", isSuperUser=" + isSuperUser);
		sb.append(", anonymous=" + anonymous);
		sb.append(")");
		
		return sb.toString();
	}
	
	
//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		String node = null;		//Atom
//		String clientId = null;		//Binary
//		String userName = null;		//Binary
//		String password = null;		//Binary
//		String peerHost = null;		//Binary
//		int sockPort = -1;		//int
//		String protocol = null;		//Atom
//		String mountPoint = null;	//Binary
//		Boolean isSuperUser = null;	//boolean
//		Boolean anonymous = null;	//boolean
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
//				case "password":
//					password = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "peerhost":
//					peerHost = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "sockport":
//					sockPort = (Integer) tuple.get(1);
//					break;
//				case "protocol":
//					protocol = CodecUtil.atom2String(tuple.get(1));
//					break;
//				case "mountpoint":
//					mountPoint = CodecUtil.binary2String(tuple.get(1));;
//					break;
//				case "is_superuser":
//					isSuperUser = (Boolean) tuple.get(1);
//					break;
//				case "anonymous":
//					anonymous = (Boolean) tuple.get(1);
//					break;
//				}
//			}
//			
//			if (node == null ||
//					clientId == null ||
//					userName == null ||
//					password == null ||
//					peerHost == null ||
//					sockPort < 0 ||
//					protocol == null ||
//					mountPoint == null ||
//					isSuperUser == null ||
//					anonymous == null) {
//				String error = MessageFormat.format("Invalid ClientInfo: {0}", object);
//				throw new InvalidParameterException(error);
//			}
//			
//			return new ClientInfo(node, clientId, userName, password, peerHost, sockPort, 
//					protocol, mountPoint, isSuperUser, anonymous);
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid ClientInfo: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//		
//	}

}
