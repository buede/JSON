package com.dreangine.json;

import static com.dreangine.json.Constants.TEXT_NULL;
import static com.dreangine.json.Constants.VALUE_COMMA;
import static com.dreangine.json.Constants.VALUE_LEFT_SQUARE_BRACKET;
import static com.dreangine.json.Constants.VALUE_QUOTATION;
import static com.dreangine.json.Constants.VALUE_RIGHT_SQUARE_BRACKET;

import java.util.ArrayList;
import java.util.Iterator;

import com.dreangine.json.error.ErrorJSONUnsupportedValue;

/**
 * @author Omar Vieira Buede
 *
 */
public class JSONArray implements Iterable<Object> {
	// Minimum length is two because of the left and right square brackets
	private static final int MIN_LENGTH = 2;
	private final ArrayList<Object> jsonArray;
	private long length;

	public JSONArray() {
		this.jsonArray = new ArrayList<>();
		this.length = MIN_LENGTH;
	}

	public void add(Object value) throws ErrorJSONUnsupportedValue {
		Object validatedObj = JSONParser.validateValue(value);
		this.jsonArray.add(validatedObj);
		this.length += (validatedObj != null ? validatedObj.toString() : TEXT_NULL).length();
		if (validatedObj instanceof String)
			this.length += 2; // Two quotation marks
		if (this.jsonArray.size() > 1)
			this.length++; // Comma separating values
	}

	public Object get(int pos) {
		return this.jsonArray.get(pos);
	}

	public Object remove(int pos) {
		Object value = this.jsonArray.remove(pos);
		if (value != null) {
			this.length -= value.toString().length();
			if (!this.jsonArray.isEmpty())
				this.length--; // Comma separating values
		}
		return value;
	}

	public int size() {
		return this.jsonArray.size();
	}

	public long length() {
		return this.length;
	}

	public void clear() {
		this.jsonArray.clear();
		this.length = MIN_LENGTH;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.jsonArray == null) ? 0 : this.jsonArray.hashCode());
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
		JSONArray other = (JSONArray) obj;
		if (this.jsonArray == null) {
			if (other.jsonArray != null)
				return false;
		} else if (!this.jsonArray.equals(other.jsonArray))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder sb;

		sb = new StringBuilder();
		sb.append(VALUE_LEFT_SQUARE_BRACKET);
		for (Object element : this.jsonArray) {
			if (sb.length() > 1)
				sb.append(VALUE_COMMA);
			if (element != null) {
				if (element instanceof String)
					sb.append(VALUE_QUOTATION);				
				sb.append(element);
				if (element instanceof String)
					sb.append(VALUE_QUOTATION);				
			}
			else
				sb.append(TEXT_NULL);
		}
		sb.append(VALUE_RIGHT_SQUARE_BRACKET);

		return sb.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<Object> iterator() {
		return this.jsonArray.iterator();
	}
}