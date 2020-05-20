package emqx.extension.java.handler.codec;

public interface HandlerReturn {
	
	public Object encode(ResultCode rc);
	
//	public ResultCode getResultCode();

}
