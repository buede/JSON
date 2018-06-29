package xyz.dreangine.json;

/**
 * @author Omar Vieira Buede
 *
 */
@SuppressWarnings("nls")
public final class Constants {
	private Constants() {
	}

	public static final String MSG_ERROR_JSON_BAD_VALUE = "This is not a valid value!";
	public static final String MSG_ERROR_JSON_UNSUPPORTED_VALUE = "This value type is not supported!";
	public static final String MSG_ERROR_JSON_INVALID = "This is not a valid JSON!";

	public static final String REGEX_NUMBER = "^[-]?\\d+([eE][-+]?\\d+)?$";
	public static final String REGEX_NUMBER_DECIMAL = "^[-]?\\d+\\.\\d+([eE][-+]?\\d+)?$";

	public static final String TEXT_NULL = "null";
	public static final String TEXT_EMPTY = "";

	public static final char VALUE_QUOTATION = '\u0022';
	public static final char VALUE_COMMA = '\u002c';
	public static final char VALUE_COLON = '\u003a';
	public static final char VALUE_LEFT_SQUARE_BRACKET = '\u005b';
	public static final char VALUE_SOLIDUS = '\u002f';
	public static final char VALUE_REVERSE_SOLIDUS = '\\';
	public static final char VALUE_RIGHT_SQUARE_BRACKET = '\u005d';
	public static final char VALUE_LEFT_CURLY_BRACKET = '\u007b';
	public static final char VALUE_RIGHT_CURLY_BRACKET = '\u007d';
}