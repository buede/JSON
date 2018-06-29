package xyz.dreangine.json;

import static xyz.dreangine.json.Constants.*;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import xyz.dreangine.json.error.ErrorJSONUnsupportedValue;

/**
 * @author Omar Vieira Buede
 *
 */
public class JSONObject {
	// Minimum length is two because of the left and right curly brackets
	private static final int MIN_LENGTH = 2;
	private final Map<String, Object> jsonMap;
	private int length;

	public JSONObject() {
		// Using LinkedHashMap because insertion order is important
		this.jsonMap = new LinkedHashMap<>();
		this.length = MIN_LENGTH;
	}

	public void add(String key, Object obj) throws ErrorJSONUnsupportedValue {
		Object validatedObj = JSONParser.validateValue(obj);
		this.jsonMap.put(key, validatedObj);
		this.length += key.length();
		this.length += 3; // Two quotation marks and one colon
		this.length += (validatedObj != null ? validatedObj.toString() : TEXT_NULL).length();
		if (validatedObj instanceof String)
			this.length += 2; // Two quotation marks
		if (this.jsonMap.size() > 1)
			this.length++; // Comma separating values
	}

	public Object get(String key) {
		return this.jsonMap.get(key);
	}

	public Object remove(String key) {
		Object value = this.jsonMap.remove(key);
		if (value != null) {
			this.length -= key.length();
			this.length -= 3; // Two quotation marks and one colon
			this.length -= value.toString().length();
			if (value instanceof String)
				this.length -= 2; // Two quotation marks
			if (!this.jsonMap.isEmpty())
				this.length--; // Comma separating values
		}
		return value;
	}

	public int size() {
		return this.jsonMap.size();
	}

	public int length() {
		return this.length;
	}

	public void clear() {
		this.jsonMap.clear();
		this.length = MIN_LENGTH;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.jsonMap == null) ? 0 : this.jsonMap.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		JSONObject other = (JSONObject) obj;
		if (this.jsonMap == null) {
			if (other.jsonMap != null)
				return false;
		} else if (!this.jsonMap.equals(other.jsonMap))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder sb;

		sb = new StringBuilder();
		sb.append(VALUE_LEFT_CURLY_BRACKET);
		for (Entry<String, Object> entry : this.jsonMap.entrySet()) {
			if (sb.length() > 1)
				sb.append(VALUE_COMMA);
			sb.append(VALUE_QUOTATION);
			sb.append(entry.getKey());
			sb.append(VALUE_QUOTATION);
			sb.append(VALUE_COLON);
			if (entry.getValue() instanceof String)
				sb.append(VALUE_QUOTATION);
			sb.append(entry.getValue());
			if (entry.getValue() instanceof String)
				sb.append(VALUE_QUOTATION);
		}
		sb.append(VALUE_RIGHT_CURLY_BRACKET);

		return sb.toString();
	}
}