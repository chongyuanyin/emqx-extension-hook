package java.extension.handler.codec;

public class TopicFilter implements HandlerParameter {

	public final String topic;		//Binary
	public final int qos;	//int
	
	public TopicFilter(String topic, int qos) {
		this.topic = topic;
		this.qos = qos;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("TopicFilter (");
		sb.append("topic=" + topic);
		sb.append(", qos=" + qos);
		sb.append(")");
		
		return sb.toString();
	}
}
