package xyz.dreangine.json;

import static xyz.dreangine.json.Constants.REGEX_NUMBER;
import static xyz.dreangine.json.Constants.REGEX_NUMBER_DECIMAL;
import static xyz.dreangine.json.Constants.TEXT_NULL;
import static xyz.dreangine.json.Constants.VALUE_COLON;
import static xyz.dreangine.json.Constants.VALUE_COMMA;
import static xyz.dreangine.json.Constants.VALUE_LEFT_CURLY_BRACKET;
import static xyz.dreangine.json.Constants.VALUE_LEFT_SQUARE_BRACKET;
import static xyz.dreangine.json.Constants.VALUE_QUOTATION;
import static xyz.dreangine.json.Constants.VALUE_REVERSE_SOLIDUS;
import static xyz.dreangine.json.Constants.VALUE_RIGHT_CURLY_BRACKET;
import static xyz.dreangine.json.Constants.VALUE_RIGHT_SQUARE_BRACKET;
import static xyz.dreangine.json.Constants.VALUE_SOLIDUS;

import xyz.dreangine.json.error.ErrorJSON;
import xyz.dreangine.json.error.ErrorJSONBadValue;
import xyz.dreangine.json.error.ErrorJSONInvalid;
import xyz.dreangine.json.error.ErrorJSONUnsupportedValue;

/**
 * @author Omar Vieira Buede
 *
 */
public class JSONParser {
	public static Object getValue(String valueStr) throws ErrorJSON {
		if (valueStr != null) {
			if (valueStr.charAt(0) == VALUE_LEFT_CURLY_BRACKET
					&& valueStr.charAt(valueStr.length() - 1) == VALUE_RIGHT_CURLY_BRACKET)
				return getJsonObject(valueStr);
			else if (valueStr.charAt(0) == VALUE_LEFT_SQUARE_BRACKET
					&& valueStr.charAt(valueStr.length() - 1) == VALUE_RIGHT_SQUARE_BRACKET)
				return getJsonArray(valueStr);
			else if (Boolean.FALSE.toString().equals(valueStr))
				return Boolean.FALSE;
			else if (Boolean.TRUE.toString().equals(valueStr))
				return Boolean.TRUE;
			else if (TEXT_NULL.equals(valueStr))
				return null;
			else if (valueStr.length() >= 2 && valueStr.charAt(0) == VALUE_QUOTATION
					&& valueStr.charAt(valueStr.length() - 1) == VALUE_QUOTATION)
				return validateString(valueStr.substring(1, valueStr.length() - 1));
			else if (valueStr.matches(REGEX_NUMBER)) {
				// It must be a number
				Number nbr = Long.valueOf(valueStr);
				if (nbr.longValue() == nbr.byteValue())
					return Byte.valueOf(valueStr);
				if (nbr.longValue() == nbr.shortValue())
					return Short.valueOf(valueStr);
				if (nbr.longValue() == nbr.intValue())
					return Integer.valueOf(valueStr);
				return nbr;
			} else if (valueStr.matches(REGEX_NUMBER_DECIMAL)) {
				// It must be a decimal number
				return Double.valueOf(valueStr);
			}
		}
		throw new ErrorJSONBadValue(valueStr);
	}

	public static JSONObject getJsonObject(String jsonStr) throws ErrorJSON {
		JSONObject jsonObj;
		String key;
		int start, openBrackets, colonCount, commaCount;
		boolean openQuotation;
		char current;

		jsonObj = new JSONObject();
		colonCount = commaCount = 0;
		openBrackets = 0;
		openQuotation = false;
		key = null;

		validateLayout(jsonStr, VALUE_LEFT_CURLY_BRACKET, VALUE_RIGHT_CURLY_BRACKET);

		// If it's not an empty JSON
		if (jsonStr.length() > 2) {
			start = 2;
			for (int i = 0; i < jsonStr.length(); i++) {
				current = jsonStr.charAt(i);

				// Skip escaped chars
				if (current == VALUE_REVERSE_SOLIDUS) {
					i = skipEscaped(jsonStr, i);
					continue;
				}

				// Quotation
				if (current == VALUE_QUOTATION)
					openQuotation = !openQuotation;
				// If quotation is open, the current char is just text
				else if (!openQuotation) {
					if (current == VALUE_LEFT_SQUARE_BRACKET || current == VALUE_LEFT_CURLY_BRACKET)
						// [ and {
						openBrackets++;
					else if (current == VALUE_RIGHT_SQUARE_BRACKET || current == VALUE_RIGHT_CURLY_BRACKET) {
						// ] and }
						openBrackets--;

						if (openBrackets == 0)
							jsonObj.add(key, getValue(jsonStr.substring(start, i)));
					} else if (current == VALUE_COLON && openBrackets == 1) {
						// :
						// only if it is on the same level as the most
						// external
						// brackets
						colonCount++;
						key = jsonStr.substring(start, i - 1);
						start = i + 1;
					} else if (current == VALUE_COMMA && openBrackets == 1) {
						// ,
						// only if it is on the same level as the most
						// external
						// brackets
						commaCount++;
						jsonObj.add(key, getValue(jsonStr.substring(start, i)));
						start = i + 2;
					}
				}
			}

			if (openQuotation || openBrackets > 0 || (colonCount > 0 && commaCount != colonCount - 1)) {
				jsonObj.clear();
				throw new ErrorJSONInvalid(jsonStr);
			}
		}
		return jsonObj;
	}

	public static JSONArray getJsonArray(String jsonStr) throws ErrorJSON {
		JSONArray jsonArray;
		int start, openBrackets;
		boolean openQuotation;
		char current;

		jsonArray = new JSONArray();
		openBrackets = 0;
		openQuotation = false;

		validateLayout(jsonStr, VALUE_LEFT_SQUARE_BRACKET, VALUE_RIGHT_SQUARE_BRACKET);

		// If it's not an empty JSON
		if (jsonStr.length() > 2) {
			start = 1;
			for (int i = 0; i < jsonStr.length(); i++) {
				current = jsonStr.charAt(i);

				// Skip escaped chars
				if (current == VALUE_REVERSE_SOLIDUS) {
					i++;
					continue;
				}

				// Quotation
				if (current == VALUE_QUOTATION)
					openQuotation = !openQuotation;
				// If quotation is open, the current char is just text
				else if (!openQuotation) {
					if (current == VALUE_LEFT_SQUARE_BRACKET || current == VALUE_LEFT_CURLY_BRACKET)
						// [ and {
						openBrackets++;
					else if (current == VALUE_RIGHT_SQUARE_BRACKET || current == VALUE_RIGHT_CURLY_BRACKET) {
						// ] and }
						openBrackets--;

						if (openBrackets == 0)
							jsonArray.add(getValue(jsonStr.substring(start, i)));
					} else if (current == VALUE_COMMA && openBrackets == 1) {
						// ,
						// only if is on the same level as the most external
						// brackets
						jsonArray.add(getValue(jsonStr.substring(start, i)));
						start = i + 1;
					}
				}
			}
		}
		return jsonArray;
	}

	public static Object validateValue(Object value) throws ErrorJSONUnsupportedValue {
		if (value != null && !(value instanceof JSONObject || value instanceof JSONArray || value instanceof String
				|| value instanceof Number || value instanceof Boolean))
			throw new ErrorJSONUnsupportedValue(value.getClass());
		return value;
	}

	public static String validateString(String str) throws ErrorJSONBadValue {
		for (int i = 0; i < str.length(); i++) {
			switch (str.charAt(i)) {
			case VALUE_REVERSE_SOLIDUS:
				// Skip escaped chars
				i = skipEscaped(str, i);
				continue;
			case VALUE_QUOTATION:
				throw new ErrorJSONBadValue(str);
			default:
				break;
			}
		}
		return str;
	}

	private static void validateLayout(String str, char first, char last) throws ErrorJSONInvalid {
		if (str == null || str.length() < 2 || str.charAt(0) != first || str.charAt(str.length() - 1) != last)
			throw new ErrorJSONInvalid(str);
	}

	private static int skipEscaped(String str, int current) throws ErrorJSONBadValue {
		switch (str.charAt(current + 1)) {
		case VALUE_QUOTATION:
		case VALUE_REVERSE_SOLIDUS:
		case VALUE_SOLIDUS:
		case 'b':
		case 'f':
		case 'n':
		case 'r':
		case 't':
			return current + 1;
		case 'u':
			return current + 5;
		default:
			throw new ErrorJSONBadValue(str);
		}
	}
}