package it.denzosoft.jprolog.builtin.xml;

// START_CHANGE: ISS-2025-0118 - XML built-in predicates
// START_CHANGE: ISS-2025-0174 - XML XXE hardening
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import javax.xml.parsers.*;
import javax.xml.xpath.*;
import org.w3c.dom.*;
import org.xml.sax.InputSource;
import java.io.StringReader;
import java.util.*;
import java.util.logging.Logger;

/**
 * XML predicates:
 *   xml_parse/2      - xml_parse(+XmlString, -Term)   parse XML to Prolog term
 *   xml_serialize/2  - xml_serialize(+Term, -XmlString) serialize term to XML
 *   xpath/3          - xpath(+XmlTerm, +XPath, -Results) XPath query
 */
public class XmlPredicates implements BuiltIn {

    private static final Logger LOGGER = Logger.getLogger(XmlPredicates.class.getName());

    public enum Mode { XML_PARSE, XML_SERIALIZE, XPATH }

    private final Mode mode;

    public XmlPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case XML_PARSE:     return doXmlParse(query, bindings, solutions);
                case XML_SERIALIZE: return doXmlSerialize(query, bindings, solutions);
                case XPATH:         return doXpath(query, bindings, solutions);
                default: return false;
            }
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            // START_CHANGE: ISS-2025-0682 - wave Q1.1: malformed XML / XPath text is a syntax
            // error of the argument, anything else a system error (never a message atom)
            int n = query.getArguments() == null ? 0 : query.getArguments().size();
            String msg = e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName();
            if (e instanceof org.xml.sax.SAXException) throw Errors.syntax("xml", modeName(), n, msg);
            if (e instanceof javax.xml.xpath.XPathExpressionException) throw Errors.syntax("xpath", modeName(), n, msg);
            throw Errors.host(e, "read", "xml", null, modeName(), n);
            // END_CHANGE: ISS-2025-0682
        }
    }

    // START_CHANGE: ISS-2025-0174 - Comprehensive XXE protection for DocumentBuilderFactory
    /**
     * Create a hardened DocumentBuilderFactory with XXE protections.
     * Each feature is set individually with try-catch so that unsupported
     * features on certain implementations do not prevent the factory from being used.
     */
    private static DocumentBuilderFactory createSecureDocumentBuilderFactory() {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        // Disable DTDs entirely
        safeSetFeature(factory, "http://apache.org/xml/features/disallow-doctype-decl", true);
        // Disable external general entities
        safeSetFeature(factory, "http://xml.org/sax/features/external-general-entities", false);
        // Disable external parameter entities
        safeSetFeature(factory, "http://xml.org/sax/features/external-parameter-entities", false);
        // Disable external DTDs
        safeSetFeature(factory, "http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
        // Prevent expansion attacks
        factory.setExpandEntityReferences(false);

        return factory;
    }

    /**
     * Safely set a feature on a DocumentBuilderFactory, logging a warning
     * if the feature is not supported by the current implementation.
     */
    private static void safeSetFeature(DocumentBuilderFactory factory, String feature, boolean value) {
        try {
            factory.setFeature(feature, value);
        } catch (ParserConfigurationException e) {
            LOGGER.warning("XML security feature not supported: " + feature + " - " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0174

    /**
     * xml_parse(+XmlString, -Term)
     * Parses XML into element(Tag, Attributes, Children) terms.
     * Attributes: [name=value, ...]
     * Children: list of element(...) or atom (text content)
     */
    private boolean doXmlParse(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2);
        String xml = resolveAtom(query, 0, bindings);

        // START_CHANGE: ISS-2025-0174 - Use hardened factory
        DocumentBuilderFactory factory = createSecureDocumentBuilderFactory();
        // END_CHANGE: ISS-2025-0174
        DocumentBuilder builder = factory.newDocumentBuilder();
        builder.setErrorHandler(SILENT);                               // ISS-2025-0682
        Document doc = builder.parse(new InputSource(new StringReader(xml)));
        Term term = domToTerm(doc.getDocumentElement());
        return unify(query.getArguments().get(1), term, bindings, solutions);
    }

    /**
     * xml_serialize(+Term, -XmlString)
     * Converts element(Tag, Attrs, Children) back to XML string.
     */
    private boolean doXmlSerialize(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        Term term = query.getArguments().get(0).resolveBindings(bindings);
        String xml = termToXml(term);                                  // ISS-2025-0682: checks it
        return unify(query.getArguments().get(1), new Atom(xml), bindings, solutions);
    }

    /**
     * xpath(+XmlString, +XPathExpr, -Results)
     * Evaluates XPath on XML string, returns list of text results.
     */
    private boolean doXpath(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3);
        String xml = resolveAtom(query, 0, bindings);
        String xpathExpr = resolveAtom(query, 1, bindings);

        // START_CHANGE: ISS-2025-0174 - Use hardened factory
        DocumentBuilderFactory factory = createSecureDocumentBuilderFactory();
        // END_CHANGE: ISS-2025-0174
        DocumentBuilder builder = factory.newDocumentBuilder();
        builder.setErrorHandler(SILENT);                               // ISS-2025-0682
        Document doc = builder.parse(new InputSource(new StringReader(xml)));

        XPathFactory xpFactory = XPathFactory.newInstance();
        XPath xpath = xpFactory.newXPath();
        NodeList nodes = (NodeList) xpath.evaluate(xpathExpr, doc, XPathConstants.NODESET);

        List<Term> results = new ArrayList<>();
        for (int i = 0; i < nodes.getLength(); i++) {
            Node node = nodes.item(i);
            String text = node.getTextContent();
            results.add(new Atom(text != null ? text : ""));
        }
        return unify(query.getArguments().get(2), CollectionUtils.createListTerm(results), bindings, solutions);
    }

    private Term domToTerm(Element elem) {
        String tag = elem.getTagName();

        // Attributes
        List<Term> attrs = new ArrayList<>();
        NamedNodeMap attrMap = elem.getAttributes();
        for (int i = 0; i < attrMap.getLength(); i++) {
            Attr attr = (Attr) attrMap.item(i);
            Term pair = new CompoundTerm(new Atom("="), Arrays.asList(new Atom(attr.getName()), new Atom(attr.getValue())));
            attrs.add(pair);
        }

        // Children
        List<Term> children = new ArrayList<>();
        NodeList childNodes = elem.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++) {
            Node child = childNodes.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                children.add(domToTerm((Element) child));
            } else if (child.getNodeType() == Node.TEXT_NODE) {
                String text = child.getTextContent().trim();
                if (!text.isEmpty()) {
                    children.add(new Atom(text));
                }
            }
        }

        return new CompoundTerm(new Atom("element"), Arrays.asList(
            new Atom(tag),
            CollectionUtils.createListTerm(attrs),
            CollectionUtils.createListTerm(children)
        ));
    }

    private String termToXml(Term term) {
        if (term instanceof Atom) {
            return escapeXml(((Atom) term).getName());
        }
        // START_CHANGE: ISS-2025-0682 - an unbound or malformed element is an argument fault
        if (term instanceof Variable) throw Errors.instantiation("xml_serialize", 2, "the element must be bound");
        if (!(term instanceof CompoundTerm) || !"element".equals(((CompoundTerm) term).getName())) {
            if (term instanceof CompoundTerm) {
                throw Errors.type("xml_element", term, "xml_serialize", 2, "element(Tag, Attributes, Children) expected");
            }
            return term.toString();
        }
        List<Term> args = term.getArguments();
        if (args.size() != 3) {
            throw Errors.type("xml_element", term, "xml_serialize", 2, "element/3 expected");
        }
        if (args.get(0) instanceof Variable) throw Errors.instantiation("xml_serialize", 2, "the tag must be bound");
        if (!(args.get(0) instanceof Atom)) throw Errors.type("atom", args.get(0), "xml_serialize", 2, "the tag must be an atom");
        // END_CHANGE: ISS-2025-0682

        String tag = ((Atom) args.get(0)).getName();
        StringBuilder sb = new StringBuilder();
        sb.append('<').append(tag);

        // Attributes
        Term attrList = args.get(1);
        List<Term> attrs = CollectionUtils.termToList(attrList);
        if (attrs != null) {
            for (Term attr : attrs) {
                if (attr instanceof CompoundTerm && "=".equals(((CompoundTerm) attr).getName())) {
                    String name = ((Atom) attr.getArguments().get(0)).getName();
                    String value = ((Atom) attr.getArguments().get(1)).getName();
                    sb.append(' ').append(name).append("=\"").append(escapeXml(value)).append('"');
                }
            }
        }

        // Children
        Term childList = args.get(2);
        List<Term> children = CollectionUtils.termToList(childList);
        if (children == null || children.isEmpty()) {
            sb.append("/>");
        } else {
            sb.append('>');
            for (Term child : children) {
                sb.append(termToXml(child));
            }
            sb.append("</").append(tag).append('>');
        }
        return sb.toString();
    }

    private String escapeXml(String s) {
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                .replace("\"", "&quot;").replace("'", "&apos;");
    }

    // START_CHANGE: ISS-2025-0682 - wave Q1.1: ISO error terms, not message atoms
    /** The default handler prints "[Fatal Error] ..." on System.err; the error term says it all. */
    private static final org.xml.sax.ErrorHandler SILENT = new org.xml.sax.ErrorHandler() {
        @Override public void warning(org.xml.sax.SAXParseException e) { }
        @Override public void error(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException { throw e; }
        @Override public void fatalError(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException { throw e; }
    };

    private void checkArity(Term query, int expected) {
        int n = query.getArguments() == null ? 0 : query.getArguments().size();
        if (n != expected) throw Errors.existence("procedure", Errors.pi(modeName(), n), modeName(), n, null);
    }

    private String resolveAtom(Term query, int i, Map<String, Term> bindings) {
        int n = query.getArguments().size();
        Term resolved = query.getArguments().get(i).resolveBindings(bindings);
        if (resolved instanceof Variable) throw Errors.instantiation(modeName(), n, "argument " + (i + 1) + " must be bound");
        if (resolved instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            return ((it.denzosoft.jprolog.core.terms.PrologString) resolved).getStringValue();
        }
        if (!(resolved instanceof Atom)) throw Errors.type("atom", resolved, modeName(), n, "argument " + (i + 1) + " must be an atom");
        return ((Atom) resolved).getName();
    }
    // END_CHANGE: ISS-2025-0682

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0174
// END_CHANGE: ISS-2025-0118
