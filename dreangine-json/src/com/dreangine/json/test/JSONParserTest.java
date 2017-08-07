package com.dreangine.json.test;

import static com.dreangine.json.Constants.MSG_ERROR_JSON_BAD_VALUE;
import static com.dreangine.json.Constants.MSG_ERROR_JSON_UNSUPPORTED_VALUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import com.dreangine.json.JSONArray;
import com.dreangine.json.JSONObject;
import com.dreangine.json.JSONParser;
import com.dreangine.json.error.ErrorJSON;
import com.dreangine.json.error.ErrorJSONBadValue;
import com.dreangine.json.error.ErrorJSONUnsupportedValue;

/**
 * @author Omar Vieira Buede
 *
 */
@SuppressWarnings({ "nls", "static-method" })
public class JSONParserTest {
	private static final String KEY_JSON_OBJECT = "Key object";
	private static final String KEY_JSON_ARRAY = "Key array";
	private static final String KEY_STRING = "Key string";
	private static final String KEY_BOOLEAN = "Key boolean";
	private static final String KEY_NUMBER = "Key number";
	private static final String KEY_NULL = "Key null";

	private static final String VALUE_JSON_OBJECT = "{}";
	private static final String VALUE_JSON_ARRAY = "[]";
	private static final String VALUE_STRING = "Value \\\"str\\\"ing\\\"";
	private static final String VALUE_STRING_WRAPPED = "\"" + VALUE_STRING + "\"";
	private static final String VALUE_STRING_MALFORMED = "Value \"str\"ing\\\"";
	private static final String VALUE_STRING_EMPTY = "";
	private static final String VALUE_BOOLEAN = Boolean.TRUE.toString();
	private static final String VALUE_NUMBER = String.valueOf(Byte.MAX_VALUE);
	private static final String VALUE_NULL = "null";

	private static final String PAIR_JSON_OBJECT = "\"" + KEY_JSON_OBJECT + "\":" + VALUE_JSON_OBJECT;
	private static final String PAIR_JSON_ARRAY = "\"" + KEY_JSON_ARRAY + "\":" + VALUE_JSON_ARRAY;
	private static final String PAIR_STRING = "\"" + KEY_STRING + "\":" + VALUE_STRING_WRAPPED;
	private static final String PAIR_BOOLEAN = "\"" + KEY_BOOLEAN + "\":" + VALUE_BOOLEAN;
	private static final String PAIR_NUMBER = "\"" + KEY_NUMBER + "\":" + VALUE_NUMBER;
	private static final String PAIR_NULL = "\"" + KEY_NULL + "\":" + VALUE_NULL;

	private static final String JSON_OBJECT_EMPTY = VALUE_JSON_OBJECT;
	private static final String JSON_OBJECT_SIMPLE = "{" + PAIR_STRING + "}";
	private static final String JSON_OBJECT_FULL = "{" + PAIR_JSON_OBJECT + "," + PAIR_JSON_ARRAY + "," + PAIR_STRING
			+ "," + PAIR_BOOLEAN + "," + PAIR_NUMBER + "," + PAIR_NULL + "}";
	private static final String JSON_OBJECT_COMPLEX = "{\"web-app\":{\"servlet\":[{\"servlet-name\":\"cofaxCDS\",\"servlet-class\":\"org.cofax.cds.CDSServlet\",\"init-param\":{\"configGlossary:installationAt\":\"Philadelphia,PA\",\"configGlossary:adminEmail\":\"ksm@pobox.com\",\"configGlossary:poweredBy\":\"Cofax\",\"configGlossary:poweredByIcon\":\"/images/cofax.gif\",\"configGlossary:staticPath\":\"/content/static\",\"templateProcessorClass\":\"org.cofax.WysiwygTemplate\",\"templateLoaderClass\":\"org.cofax.FilesTemplateLoader\",\"templatePath\":\"templates\",\"templateOverridePath\":\"\",\"defaultListTemplate\":\"listTemplate.htm\",\"defaultFileTemplate\":\"articleTemplate.htm\",\"useJSP\":false,\"jspListTemplate\":\"listTemplate.jsp\",\"jspFileTemplate\":\"articleTemplate.jsp\",\"cachePackageTagsTrack\":200,\"cachePackageTagsStore\":200,\"cachePackageTagsRefresh\":60,\"cacheTemplatesTrack\":100,\"cacheTemplatesStore\":50,\"cacheTemplatesRefresh\":15,\"cachePagesTrack\":200,\"cachePagesStore\":100,\"cachePagesRefresh\":10,\"cachePagesDirtyRead\":10,\"searchEngineListTemplate\":\"forSearchEnginesList.htm\",\"searchEngineFileTemplate\":\"forSearchEngines.htm\",\"searchEngineRobotsDb\":\"WEB-INF/robots.db\",\"useDataStore\":true,\"dataStoreClass\":\"org.cofax.SqlDataStore\",\"redirectionClass\":\"org.cofax.SqlRedirection\",\"dataStoreName\":\"cofax\",\"dataStoreDriver\":\"com.microsoft.jdbc.sqlserver.SQLServerDriver\",\"dataStoreUrl\":\"jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon\",\"dataStoreUser\":\"sa\",\"dataStorePassword\":\"dataStoreTestQuery\",\"dataStoreTestQuery\":\"SET NOCOUNT ON;select test='test';\",\"dataStoreLogFile\":\"/usr/local/tomcat/logs/datastore.log\",\"dataStoreInitConns\":10,\"dataStoreMaxConns\":100,\"dataStoreConnUsageLimit\":100,\"dataStoreLogLevel\":\"debug\",\"maxUrlLength\":500}},{\"servlet-name\":\"cofaxEmail\",\"servlet-class\":\"org.cofax.cds.EmailServlet\",\"init-param\":{\"mailHost\":\"mail1\",\"mailHostOverride\":\"mail2\"}},{\"servlet-name\":\"cofaxAdmin\",\"servlet-class\":\"org.cofax.cds.AdminServlet\"},{\"servlet-name\":\"fileServlet\",\"servlet-class\":\"org.cofax.cds.FileServlet\"},{\"servlet-name\":\"cofaxTools\",\"servlet-class\":\"org.cofax.cms.CofaxToolsServlet\",\"init-param\":{\"templatePath\":\"toolstemplates/\",\"log\":1,\"logLocation\":\"/usr/local/tomcat/logs/CofaxTools.log\",\"logMaxSize\":\"\",\"dataLog\":1,\"dataLogLocation\":\"/usr/local/tomcat/logs/dataLog.log\",\"dataLogMaxSize\":\"\",\"removePageCache\":\"/content/admin/remove?cache=pages&id=\",\"removeTemplateCache\":\"/content/admin/remove?cache=templates&id=\",\"fileTransferFolder\":\"/usr/local/tomcat/webapps/content/fileTransferFolder\",\"lookInContext\":1,\"adminGroupID\":4,\"betaServer\":true}}],\"servlet-mapping\":{\"cofaxCDS\":\"/\",\"cofaxEmail\":\"/cofaxutil/aemail/*\",\"cofaxAdmin\":\"/admin/*\",\"fileServlet\":\"/static/*\",\"cofaxTools\":\"/tools/*\"},\"taglib\":{\"taglib-uri\":\"cofax.tld\",\"taglib-location\":\"/WEB-INF/tlds/cofax.tld\"}}}";
	private static final String JSON_ARRAY_EMPTY = VALUE_JSON_ARRAY;
	private static final String JSON_ARRAY_SIMPLE = "[" + VALUE_STRING_WRAPPED + "]";
	private static final String JSON_ARRAY_FULL = "[" + VALUE_JSON_OBJECT + "," + VALUE_JSON_ARRAY + ","
			+ VALUE_STRING_WRAPPED + "," + VALUE_BOOLEAN + "," + VALUE_NUMBER + "," + VALUE_NULL + "]";

	@Rule
	public ExpectedException thrown = ExpectedException.none();
	
	/**
	 * Test method for
	 * {@link com.dreangine.json.JSONParser#getValue(java.lang.Object)}.
	 * 
	 * @throws ErrorJSON
	 */
	@Test
	public void testGetValue() throws ErrorJSON {
		// String
		assertEquals(VALUE_STRING, JSONParser.getValue(VALUE_STRING_WRAPPED));

		// Boolean
		assertEquals(Boolean.TRUE, JSONParser.getValue(Boolean.TRUE.toString()));
		assertEquals(Boolean.FALSE, JSONParser.getValue(Boolean.FALSE.toString()));

		// Number
		assertEquals(Byte.valueOf(Byte.MIN_VALUE), JSONParser.getValue(String.valueOf(Byte.MIN_VALUE)));
		assertEquals(Byte.valueOf(Byte.MAX_VALUE), JSONParser.getValue(String.valueOf(Byte.MAX_VALUE)));
		assertEquals(Short.valueOf(Short.MIN_VALUE), JSONParser.getValue(String.valueOf(Short.MIN_VALUE)));
		assertEquals(Short.valueOf(Short.MAX_VALUE), JSONParser.getValue(String.valueOf(Short.MAX_VALUE)));
		assertEquals(Integer.valueOf(Integer.MIN_VALUE), JSONParser.getValue(String.valueOf(Integer.MIN_VALUE)));
		assertEquals(Integer.valueOf(Integer.MAX_VALUE), JSONParser.getValue(String.valueOf(Integer.MAX_VALUE)));
		assertEquals(Long.valueOf(Long.MIN_VALUE), JSONParser.getValue(String.valueOf(Long.MIN_VALUE)));
		assertEquals(Long.valueOf(Long.MAX_VALUE), JSONParser.getValue(String.valueOf(Long.MAX_VALUE)));
		assertEquals(Double.valueOf(String.valueOf(Float.MIN_VALUE)),
				JSONParser.getValue(String.valueOf(Float.MIN_VALUE)));
		assertEquals(Double.valueOf(String.valueOf(Float.MAX_VALUE)),
				JSONParser.getValue(String.valueOf(Float.MAX_VALUE)));
		assertEquals(Double.valueOf(Double.MIN_VALUE), JSONParser.getValue(String.valueOf(Double.MIN_VALUE)));
		assertEquals(Double.valueOf(Double.MAX_VALUE), JSONParser.getValue(String.valueOf(Double.MAX_VALUE)));

		// JSONObject
		assertEquals(new JSONObject(), JSONParser.getValue(VALUE_JSON_OBJECT));

		// JSONArray
		assertEquals(new JSONArray(), JSONParser.getValue(VALUE_JSON_ARRAY));
	}

	/**
	 * Test method for
	 * {@link com.dreangine.json.JSONParser#getJsonObject(java.lang.String)}.
	 * 
	 * @throws ErrorJSON
	 */
	@Test
	public void testGetJsonObjectString() throws ErrorJSON {
		JSONObject jObj;

		jObj = new JSONObject();
		jObj.add(KEY_STRING, VALUE_STRING);
		assertEquals(jObj, JSONParser.getJsonObject(JSON_OBJECT_SIMPLE));

		jObj = new JSONObject();
		jObj.add(KEY_JSON_OBJECT, new JSONObject());
		jObj.add(KEY_JSON_ARRAY, new JSONArray());
		jObj.add(KEY_STRING, VALUE_STRING);
		jObj.add(KEY_BOOLEAN, Boolean.valueOf(VALUE_BOOLEAN));
		jObj.add(KEY_NUMBER, Byte.valueOf(VALUE_NUMBER));
		jObj.add(KEY_NULL, null);
		assertEquals(jObj, JSONParser.getJsonObject(JSON_OBJECT_FULL));

		assertEquals(new JSONObject(), JSONParser.getJsonObject(JSON_OBJECT_EMPTY));
		assertEquals(JSON_OBJECT_COMPLEX, JSONParser.getJsonObject(JSON_OBJECT_COMPLEX).toString());
	}

	/**
	 * Test method for
	 * {@link com.dreangine.json.JSONParser#getJsonArray(java.lang.String)}.
	 * 
	 * @throws ErrorJSON
	 */
	@Test
	public void testGetJsonArrayString() throws ErrorJSON {
		JSONArray jArray;

		jArray = new JSONArray();
		jArray.add(VALUE_STRING);
		assertEquals(jArray, JSONParser.getJsonArray(JSON_ARRAY_SIMPLE));

		jArray = new JSONArray();
		jArray.add(new JSONObject());
		jArray.add(new JSONArray());
		jArray.add(VALUE_STRING);
		jArray.add(Boolean.valueOf(VALUE_BOOLEAN));
		jArray.add(Byte.valueOf(VALUE_NUMBER));
		jArray.add(null);
		assertEquals(jArray, JSONParser.getJsonArray(JSON_ARRAY_FULL));

		assertEquals(new JSONArray(), JSONParser.getJsonArray(JSON_ARRAY_EMPTY));
	}

	/**
	 * Test method for
	 * {@link com.dreangine.json.JSONParser#validateValue(java.lang.Object)}.
	 * 
	 * @throws ErrorJSON
	 */
	@Test
	public void testValidateValue() throws ErrorJSON {
		// Null
		assertNull(JSONParser.validateValue(null));

		// String
		assertEquals(VALUE_STRING, JSONParser.validateValue(VALUE_STRING));
		assertEquals(VALUE_STRING_EMPTY, JSONParser.validateValue(VALUE_STRING_EMPTY));

		// Boolean
		assertEquals(Boolean.TRUE, JSONParser.validateValue(Boolean.TRUE));
		assertEquals(Boolean.FALSE, JSONParser.validateValue(Boolean.FALSE));

		// Number
		assertEquals(Byte.valueOf(Byte.MIN_VALUE), JSONParser.validateValue(Byte.valueOf(Byte.MIN_VALUE)));
		assertEquals(Byte.valueOf(Byte.MAX_VALUE), JSONParser.validateValue(Byte.valueOf(Byte.MAX_VALUE)));
		assertEquals(Short.valueOf(Short.MIN_VALUE), JSONParser.validateValue(Short.valueOf(Short.MIN_VALUE)));
		assertEquals(Short.valueOf(Short.MAX_VALUE), JSONParser.validateValue(Short.valueOf(Short.MAX_VALUE)));
		assertEquals(Integer.valueOf(Integer.MIN_VALUE), JSONParser.validateValue(Integer.valueOf(Integer.MIN_VALUE)));
		assertEquals(Integer.valueOf(Integer.MAX_VALUE), JSONParser.validateValue(Integer.valueOf(Integer.MAX_VALUE)));
		assertEquals(Long.valueOf(Long.MIN_VALUE), JSONParser.validateValue(Long.valueOf(Long.MIN_VALUE)));
		assertEquals(Long.valueOf(Long.MAX_VALUE), JSONParser.validateValue(Long.valueOf(Long.MAX_VALUE)));
		assertEquals(Float.valueOf(Float.MIN_VALUE), JSONParser.validateValue(Float.valueOf(Float.MIN_VALUE)));
		assertEquals(Float.valueOf(Float.MAX_VALUE), JSONParser.validateValue(Float.valueOf(Float.MAX_VALUE)));
		assertEquals(Double.valueOf(Double.MIN_VALUE), JSONParser.validateValue(Double.valueOf(Double.MIN_VALUE)));
		assertEquals(Double.valueOf(Double.MAX_VALUE), JSONParser.validateValue(Double.valueOf(Double.MAX_VALUE)));

		// JSONObject
		assertEquals(new JSONObject(), JSONParser.validateValue(new JSONObject()));

		// JSONArray
		assertEquals(new JSONArray(), JSONParser.validateValue(new JSONArray()));
		
		this.thrown.expect(ErrorJSONUnsupportedValue.class);
		this.thrown.expectMessage(MSG_ERROR_JSON_UNSUPPORTED_VALUE);
		JSONParser.validateValue(new ErrorJSON(null));
	}

	/**
	 * Test method for
	 * {@link com.dreangine.json.JSONParser#validateString(String)}.
	 * 
	 * @throws ErrorJSON
	 */
	@Test
	public void testValidateString() throws ErrorJSON {
		assertEquals(VALUE_STRING, JSONParser.validateString(VALUE_STRING));
		
		this.thrown.expect(ErrorJSONBadValue.class);
		this.thrown.expectMessage(MSG_ERROR_JSON_BAD_VALUE);
		JSONParser.validateString(VALUE_STRING_MALFORMED);
	}
}
