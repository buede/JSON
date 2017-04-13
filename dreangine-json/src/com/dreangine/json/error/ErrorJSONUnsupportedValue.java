package com.dreangine.json.error;

import static com.dreangine.json.Constants.MSG_ERROR_JSON_UNSUPPORTED_VALUE;

/**
 * @author Omar Vieira Buede
 *
 */
public class ErrorJSONUnsupportedValue extends ErrorJSON {
	private static final long serialVersionUID = -177636643246148548L;
	private final Class<?> c;

	public ErrorJSONUnsupportedValue(Class<?> c) {
		super(MSG_ERROR_JSON_UNSUPPORTED_VALUE);
		this.c = c;
	}

	public Class<?> getC() {
		return this.c;
	}
}