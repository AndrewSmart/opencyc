/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opencyc.api;

import java.io.IOException;
import java.net.UnknownHostException;
import java.text.FieldPosition;
import java.text.ParseException;
import java.text.ParsePosition;
import org.junit.*;
import static org.junit.Assert.*;
import org.opencyc.cycobject.CycList;

/**
 *
 * @author daves
 */
public class CycLFormatTest {
    private CycLFormat instance;
    private String newLine;
    
    public CycLFormatTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }
    
    @Before
    public void setUp() throws UnknownHostException, IOException {
        CycAccess cyc = new CycAccess("localhost", 3600);
        instance = CycLFormat.getInstance(cyc);
        // (princ-to-string #\\Newline) does not work, because CycList parser cannot handle #\ ... :P
        newLine = cyc.converseString("(clet (result) (cwith-output-to-string (s result) (terpri s)) (identity result))");
    }
    
    @After
    public void tearDown() {
    }

    /**
     * Test of getInstance method, of class CycLFormat.
     */
    //@Test
    public void testGetInstance() {
        System.out.println("getInstance");
        CycAccess cyc = null;
        CycLFormat expResult = null;
        CycLFormat result = CycLFormat.getInstance(cyc);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setWrapLines method, of class CycLFormat.
     */
    //@Test
    public void testSetWrapLines() {
        System.out.println("setWrapLines");
        boolean newlines = false;
        CycLFormat instance = null;
        instance.setWrapLines(newlines);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setShowHashDollar method, of class CycLFormat.
     */
    //@Test
    public void testSetShowHashDollar() {
        System.out.println("setShowHashDollar");
        boolean showHashDollar = false;
        CycLFormat instance = null;
        instance.setShowHashDollar(showHashDollar);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }



    /**
     * Test of parseObject method, of class CycLFormat.
     */
    @Test
    public void testParseObject() {
        System.out.println("testParseObject");
        String source = "(#$implies (#$isa ?X #$Cat) (#$likesAsFriend ?X #$CycAdministrator))";
        ParsePosition pos = new ParsePosition(0);
        Object expResult = null;
        Object result = instance.parseObject(source, pos);
        assertTrue(result instanceof CycList);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
        /**
     * Test of format method, of class CycLFormat.
     */
    @Test
    public void testFormat() throws ParseException {
        System.out.println("format");
        Object obj = instance.parseObject("(#$implies (#$isa ?X #$Cat) (#$likesAsFriend ?X #$CycAdministrator))", new ParsePosition(0));
        StringBuffer toAppendTo = new StringBuffer();
        FieldPosition pos = null;
        String expResult = "(#$implies (#$isa ?X #$Cat) (#$likesAsFriend ?X #$CycAdministrator))";
        instance.setShowHashDollar(true);
        instance.setWrapLines(false);
        StringBuffer result = instance.format(obj, toAppendTo, pos);
        assertEquals(expResult, result.toString());
        // TODO review the generated test code and remove the default call to fail.
        
    }
    
    @Test
    public void testFormatNoHashDollar() throws ParseException {
        System.out.println("formatNoHashDollar");
        Object obj = instance.parseObject("(#$implies (#$isa ?X #$Cat) (#$likesAsFriend ?X #$CycAdministrator))", new ParsePosition(0));
        StringBuffer toAppendTo = new StringBuffer();
        FieldPosition pos = null;
        String expResult = "(implies (isa ?X Cat) (likesAsFriend ?X CycAdministrator))";
        instance.setShowHashDollar(false);
        instance.setWrapLines(false);
        StringBuffer result = instance.format(obj, toAppendTo, pos);
        assertEquals(expResult, result.toString());
        // TODO review the generated test code and remove the default call to fail.
        
    }
    
    @Test
    public void testFormatWrap() throws ParseException {
        System.out.println("formatWrap");
        Object obj = instance.parseObject("(#$implies (#$isa ?X #$Cat) (#$likesAsFriend ?X #$CycAdministrator))", new ParsePosition(0));
        StringBuffer toAppendTo = new StringBuffer();
        FieldPosition pos = null;
        String expResult = "(#$implies " + newLine +"  (#$isa ?X #$Cat) " + newLine + "  (#$likesAsFriend ?X #$CycAdministrator))";
        instance.setShowHashDollar(true);
        instance.setWrapLines(true);
        StringBuffer result = instance.format(obj, toAppendTo, pos);
        assertEquals(expResult, result.toString());
        // TODO review the generated test code and remove the default call to fail.
    }
    
    @Test
    public void testFormatWrapNoHashDollar() throws ParseException {
        System.out.println("formatWrapNoHashDollar");
        Object obj = instance.parseObject("(#$implies (#$isa ?X #$Cat) (#$likesAsFriend ?X #$CycAdministrator))", new ParsePosition(0));
        StringBuffer toAppendTo = new StringBuffer();
        FieldPosition pos = null;
        String expResult = "(implies " + newLine + "  (isa ?X Cat) " + newLine + "  (likesAsFriend ?X CycAdministrator))";
        instance.setShowHashDollar(false);
        instance.setWrapLines(true);
        StringBuffer result = instance.format(obj, toAppendTo, pos);
        assertEquals(expResult, result.toString());
        // TODO review the generated test code and remove the default call to fail.
        
    }
}
