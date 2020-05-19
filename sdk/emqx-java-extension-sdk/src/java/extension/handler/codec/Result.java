package java.extension.handler.codec;

import erlport.terms.Tuple;

public class Result implements HandlerReturn {
	
	public final boolean value;		//boolean
	
//	private ResultCode resultCode = ResultCode.SUC;
	
	public Result(boolean value) {
		this.value = value;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Result (");
		sb.append("value=" + value);
		sb.append(")");
		
		return sb.toString();
	}

	@Override
	public Object encode(ResultCode rc) {
		return Tuple.two(rc.getValue(), value);
	}

//	@Override
//	public ResultCode getResultCode() {
//		return resultCode;
//	}
//	
//	public void setResultCode(ResultCode rc) {
//		this.resultCode = rc;
//	}

}
