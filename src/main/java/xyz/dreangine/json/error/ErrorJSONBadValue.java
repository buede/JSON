package xyz.dreangine.json.error;

import static xyz.dreangine.json.Constants.MSG_ERROR_JSON_BAD_VALUE;

/**
 * @author Omar Vieira Buede
 *
 */
public class ErrorJSONBadValue extends ErrorJSON {
	private static final long serialVersionUID = -664807372088860093L;
	private final String value;

	public ErrorJSONBadValue(String value) {
		super(MSG_ERROR_JSON_BAD_VALUE);
		this.value = value;
	}

	public String getValue() {
		return this.value;
	}
}