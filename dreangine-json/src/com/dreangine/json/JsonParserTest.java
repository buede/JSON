package com.dreangine.json;

import static org.testng.Assert.*;

/**
 * Created by omar on 09/04/2017.
 */
public class JsonParserTest {
    private static final String key = "Key";
    private static final String valueStr = "Value";

    @org.testng.annotations.Test
    public void testGetJsonObject() throws Exception {
        JsonObject jsonObjTarget = new JsonObject();
        jsonObjTarget.add(key, valueStr);

        JsonObject jsonObjGenerated = JsonParser.getJsonObject("{\"" + key +
                "\":\"" + valueStr +
                "\"}");
        assertEquals(jsonObjTarget, jsonObjGenerated);
    }

    @org.testng.annotations.Test
    public void testGetJsonObject1() throws Exception {
    }

    @org.testng.annotations.Test
    public void testGetJsonObject2() throws Exception {
    }

    @org.testng.annotations.Test
    public void testGetJsonArray() throws Exception {
    }

    @org.testng.annotations.Test
    public void testGetJsonArray1() throws Exception {
    }
}