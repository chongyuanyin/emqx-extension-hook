package java.extension.handler.codec;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import erlport.terms.Atom;
import erlport.terms.Binary;
import erlport.terms.Tuple;

public class Message implements HandlerParameter, HandlerReturn {
	
	public final String node;		//Atom
	public final String id;		//Binary
	public final int qos;			//int
	public final String from;		//Binary
	public final String topic;	//Binary
	public final byte[] payload;	//Binary
	public final long timestamp;	//long
	
//	private ResultCode resultCode = ResultCode.SUC;
	
	public Message(String node, String id, int qos, String from, String topic, byte[] payload, long timestamp) {
		this.node = node;
		this.id = id;
		this.qos = qos;
		this.from = from;
		this.topic = topic;
		this.payload = payload;
		this.timestamp = timestamp;
	}

	@Override
	public Tuple encode(ResultCode rc) {
		List<Tuple> tuples = new ArrayList<>();
		
		tuples.add(Tuple.two("node", new Atom(node)));
		tuples.add(Tuple.two("id", new Binary(id)));
		tuples.add(Tuple.two("qos", qos));
		tuples.add(Tuple.two("from", new Binary(from)));
		tuples.add(Tuple.two("topic", new Binary(topic)));
		tuples.add(Tuple.two("payload", new Binary(payload)));
		tuples.add(Tuple.two("timestamp", BigInteger.valueOf(timestamp)));
		
		return Tuple.two(rc.getValue(), tuples);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Message (");
		sb.append("node=" + node);
		sb.append(", id=" + id);
		sb.append(", qos=" + qos);
		sb.append(", from=" + from);
		sb.append(", topic=" + topic);
		sb.append(", payload=" + payload);
		sb.append(", timestamp=" + timestamp);
		sb.append(")");
		
		return sb.toString();
	}

//	@Override
//	public ResultCode getResultCode() {
//		return resultCode;
//	}
//	
//	public void setResultCode(ResultCode rc) {
//		this.resultCode = rc;
//	}
	
//	@Override
//	public HandlerParameter parse(Object object) throws InvalidParameterException {
//		String node = null;		//Atom
//		String id = null;		//Binary
//		int qos = -1;			//int
//		String from = null;		//Binary
//		String topic = null;	//Binary
//		byte[] payload = null;	//Binary
//		long timestamp = -1;
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
//				case "id":
//					id = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "qos":
//					qos = (Integer)(tuple.get(1));
//					break;
//				case "from":
//					from = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "topic":
//					topic = CodecUtil.binary2String(tuple.get(1));
//					break;
//				case "payload":
//					payload = CodecUtil.binary2ByteArray(tuple.get(1));
//					break;
//				case "timestamp":
//					timestamp = (Long)(tuple.get(1));
//					break;
//				}
//			}
//			
//			if (node == null ||
//					id == null ||
//					qos < 0 ||
//					from == null ||
//					topic == null ||
//					payload == null ||
//					timestamp < 0) {
//				String error = MessageFormat.format("Invalid Message: {0}", object);
//				throw new InvalidParameterException(error);
//			}
//			
//			return new Message(node, id, qos, from, topic, payload, timestamp);
//			
//		} catch (Exception e) {
//			String error = MessageFormat.format("Invalid Message: {0}", object);
//			throw new InvalidParameterException(error);
//		}
//			
//	}
}
