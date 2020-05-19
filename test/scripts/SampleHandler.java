import java.extension.handler.ActionOptionConfig;
import java.extension.handler.ActionOptionConfig.Keys;
import java.extension.handler.DefaultCommunicationHandler;
import java.extension.handler.codec.ClientInfo;
import java.extension.handler.codec.ConnInfo;
import java.extension.handler.codec.Message;
import java.extension.handler.codec.Property;
import java.extension.handler.codec.PubSub;
import java.extension.handler.codec.Reason;
import java.extension.handler.codec.Result;
import java.extension.handler.codec.ReturnCode;
import java.extension.handler.codec.State;
import java.extension.handler.codec.SubscribeOption;
import java.extension.handler.codec.Topic;
import java.extension.handler.codec.TopicFilter;

public class SampleHandler extends DefaultCommunicationHandler {
	
	@Override
	public ActionOptionConfig getActionOption() {
		ActionOptionConfig option = new ActionOptionConfig();
		option.set(Keys.MESSAGE_PUBLISH_TOPICS, "t_sample,topic_sample");
		option.set(Keys.MESSAGE_DELIVERED_TOPICS, "t_sample,topic_sample");
		option.set(Keys.MESSAGE_ACKED_TOPICS, "t_sample,topic_sample");
		option.set(Keys.MESSAGE_DROPPED_TOPICS, "t_sample,topic_sample");
		
		return option;
	}
	
	// Clients
	@Override
    public void on_client_connect(ConnInfo connInfo, Property[] props, State state) {
        System.err.printf("[Java] on_client_connect: connInfo: %s, props: %s, state: %s\n", connInfo, props, state);
    }

	@Override
    public void on_client_connack(ConnInfo connInfo, ReturnCode rc, Property[] props, State state) {
        System.err.printf("[Java] on_client_connack: connInfo: %s, rc: %s, props: %s, state: %s\n", connInfo, rc, props, state);
    }

	@Override
    public void on_client_connected(ClientInfo clientInfo, State state) {
        System.err.printf("[Java] on_client_connected: clientinfo: %s, state: %s\n", clientInfo, state);
    }

	@Override
    public void on_client_disconnected(ClientInfo clientInfo, Reason reason, State state) {
        System.err.printf("[Java] on_client_disconnected: clientinfo: %s, reason: %s, state: %s\n", clientInfo, reason, state);
    }

	@Override
    public Result on_client_authenticate(ClientInfo clientInfo, boolean authresult, State state) {
        System.err.printf("[Java] on_client_authenticate: clientinfo: %s, authresult: %s, state: %s\n", clientInfo, authresult, state);

        return new Result(true);
    }

	@Override
    public Result on_client_check_acl(ClientInfo clientInfo, PubSub pubsub, Topic topic, boolean result, State state) {
        System.err.printf("[Java] on_client_check_acl: clientinfo: %s, pubsub: %s, topic: %s, result: %s, state: %s\n", clientInfo, pubsub, topic, result, state);

        return new Result(true);
    }

	@Override
    public void on_client_subscribe(ClientInfo clientInfo, Property[] props, TopicFilter[] topic, State state) {
        System.err.printf("[Java] on_client_subscribe: clientinfo: %s, topic: %s, props: %s, state: %s\n", clientInfo, topic, props, state);
    }

	@Override
    public void on_client_unsubscribe(ClientInfo clientInfo, Property[] props, TopicFilter[] topic, State state) {
        System.err.printf("[Java] on_client_unsubscribe: clientinfo: %s, topic: %s, props: %s, state: %s\n", clientInfo, topic, props, state);
    }

    // Sessions
	@Override
    public void on_session_created(ClientInfo clientInfo, State state) {
        System.err.printf("[Java] on_session_created: clientinfo: %s, state: %s\n", clientInfo, state);
    }

	@Override
    public void on_session_subscribed(ClientInfo clientInfo, Topic topic, SubscribeOption opts, State state) {
        System.err.printf("[Java] on_session_subscribed: clientinfo: %s, topic: %s, state: %s\n", clientInfo, topic, state);
    }

	@Override
    public void on_session_unsubscribed(ClientInfo clientInfo, Topic topic, State state) {
        System.err.printf("[Java] on_session_unsubscribed: clientinfo: %s, topic: %s, state: %s\n", clientInfo, topic, state);
    }

	@Override
    public void on_session_resumed(ClientInfo clientInfo, State state) {
        System.err.printf("[Java] on_session_resumed: clientinfo: %s, state: %s\n", clientInfo, state);
    }

	@Override
    public void on_session_discarded(ClientInfo clientInfo, State state) {
        System.err.printf("[Java] on_session_discarded: clientinfo: %s, state: %s\n", clientInfo, state);
    }
	
	@Override
    public void on_session_takeovered(ClientInfo clientInfo, State state) {
        System.err.printf("[Java] on_session_takeovered: clientinfo: %s, state: %s\n", clientInfo, state);
    }

	@Override
    public void on_session_terminated(ClientInfo clientInfo, Reason reason, State state) {
        System.err.printf("[Java] on_session_terminated: clientinfo: %s, reason: %s, state: %s\n", clientInfo, reason, state);
    }

    // Messages
	@Override
    public Message on_message_publish(Message message, State state) {
        System.err.printf("[Java] on_message_publish: message: %s, state: %s\n", message, state);
        
        return message;
    }

	@Override
    public void on_message_dropped(Message message, Reason reason, State state) {
        System.err.printf("[Java] on_message_dropped: message: %s, reason: %s, state: %s\n", message, reason, state);
    }

	@Override
    public void on_message_delivered(ClientInfo clientInfo, Message message, State state) {
        System.err.printf("[Java] on_message_delivered: clientinfo: %s, message: %s, state: %s\n", clientInfo, message, state);
    }

	@Override
    public void on_message_acked(ClientInfo clientInfo, Message message, State state) {
        System.err.printf("[Java] on_message_acked: clientinfo: %s, message: %s, state: %s\n", clientInfo, message, state);
    }

}
