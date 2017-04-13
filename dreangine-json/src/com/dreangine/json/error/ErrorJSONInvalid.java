package com.dreangine.json.error;

import static com.dreangine.json.Constants.MSG_ERROR_JSON_INVALID;

/**
 * @author Omar Vieira Buede
 *
 */
public class ErrorJSONInvalid extends ErrorJSON {
	private static final long serialVersionUID = -316035652256449921L;
	private final String json;

	public ErrorJSONInvalid(String json) {
		super(MSG_ERROR_JSON_INVALID);
		this.json = json;
	}

	public String getJson() {
		return this.json;
	}
}