package it.denzosoft.jprolog.core.engine.v4;

import java.io.PrintStream;
import java.util.Locale;

// START_CHANGE: ISS-2025-0714 - wave Q2.6: the column of the console and of the capture streams.
/**
 * A {@link PrintStream} that forwards everything to a target stream unchanged (same encoding —
 * every text method delegates to the target's own text method) and remembers the column the
 * output has reached: the number of code points written since the last newline.
 *
 * <p>A file stream counts its line position in {@link PrologStream}'s byte counter, but
 * {@code user_output}/{@code user_error} are the live {@code System.out}/{@code System.err} and
 * {@code with_output_to/2} captures through a thread-local stream, so neither had a column:
 * {@code line_position(user_output, P)} was always 0 and {@code format/2}'s column stops
 * ({@code ~t~20|}) counted from the start of the format call instead of from the stream's column
 * (LIM-043). {@link Streams} wraps the system streams in one of these (re-wrapping when a test or
 * console swaps {@code System.out}), {@code with_output_to/2} wraps its capture, and
 * {@link Streams#columnOf(PrintStream)} reads the column back.
 */
public final class ColumnPrintStream extends PrintStream {

    private final PrintStream target;
    private final boolean closeTarget;
    private volatile long column;

    /**
     * @param target      the stream written to
     * @param closeTarget whether {@link #close()} closes the target (never for System.out/err)
     */
    public ColumnPrintStream(PrintStream target, boolean closeTarget) {
        super(target, false);
        this.target = target;
        this.closeTarget = closeTarget;
    }

    /** The stream this one forwards to. */
    public PrintStream target() { return target; }

    /** The 0-based column the output has reached. */
    public long column() { return column; }

    private void track(CharSequence s) {
        int n = s.length();
        int nl = -1;
        for (int i = n - 1; i >= 0; i--) {
            if (s.charAt(i) == '\n') { nl = i; break; }
        }
        long cps = 0;
        for (int i = nl + 1; i < n; i++) {
            if (!Character.isLowSurrogate(s.charAt(i))) cps++;
        }
        column = (nl >= 0) ? cps : column + cps;
    }

    private void trackByte(int b) {
        if (b == '\n') column = 0;
        else if ((b & 0xC0) != 0x80) column++;             // count UTF-8 lead bytes only
    }

    @Override public void write(int b) { target.write(b); trackByte(b & 0xFF); }

    @Override public void write(byte[] b, int off, int len) {
        target.write(b, off, len);
        for (int i = 0; i < len; i++) trackByte(b[off + i] & 0xFF);
    }

    @Override public void print(String s) { String x = String.valueOf(s); target.print(x); track(x); }
    @Override public void print(char c) { target.print(c); if (c == '\n') column = 0; else if (!Character.isLowSurrogate(c)) column++; }
    @Override public void print(char[] s) { print(new String(s)); }
    @Override public void print(Object o) { print(String.valueOf(o)); }
    @Override public void print(boolean b) { print(String.valueOf(b)); }
    @Override public void print(int i) { print(String.valueOf(i)); }
    @Override public void print(long l) { print(String.valueOf(l)); }
    @Override public void print(float f) { print(String.valueOf(f)); }
    @Override public void print(double d) { print(String.valueOf(d)); }

    @Override public void println() { target.println(); column = 0; }
    @Override public void println(String s) { target.println(s); column = 0; }
    @Override public void println(char c) { target.println(c); column = 0; }
    @Override public void println(char[] s) { target.println(s); column = 0; }
    @Override public void println(Object o) { target.println(o); column = 0; }
    @Override public void println(boolean b) { target.println(b); column = 0; }
    @Override public void println(int i) { target.println(i); column = 0; }
    @Override public void println(long l) { target.println(l); column = 0; }
    @Override public void println(float f) { target.println(f); column = 0; }
    @Override public void println(double d) { target.println(d); column = 0; }

    @Override public PrintStream append(CharSequence csq) { print(String.valueOf(csq)); return this; }
    @Override public PrintStream append(CharSequence csq, int start, int end) {
        print(String.valueOf(csq == null ? "null" : csq).substring(start, end));
        return this;
    }
    @Override public PrintStream append(char c) { print(c); return this; }

    @Override public PrintStream format(String fmt, Object... args) { print(String.format(fmt, args)); return this; }
    @Override public PrintStream format(Locale l, String fmt, Object... args) { print(String.format(l, fmt, args)); return this; }
    @Override public PrintStream printf(String fmt, Object... args) { return format(fmt, args); }
    @Override public PrintStream printf(Locale l, String fmt, Object... args) { return format(l, fmt, args); }

    @Override public void flush() { target.flush(); }
    @Override public void close() { if (closeTarget) target.close(); else target.flush(); }
    @Override public boolean checkError() { return target.checkError(); }
}
// END_CHANGE: ISS-2025-0714
