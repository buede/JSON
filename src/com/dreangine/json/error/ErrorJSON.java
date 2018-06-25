package com.dreangine.json.error;

/**
 * @author Omar Vieira Buede
 *
 */
public class ErrorJSON extends Throwable {
	private static final long serialVersionUID = 7266390447663593801L;

	public ErrorJSON(String message) {
		super(message);
	}
}