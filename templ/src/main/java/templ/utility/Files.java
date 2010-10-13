package templ.utility;

import java.io.*;
import java.net.URI;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.List;
import java.util.ArrayList;

/**
 * This is a convenience class with static methods for reading and writing files without all the fuzz.
 */
public final class Files {
    /** Reads an entire file as a string. */
    public static String read(Reader reader) {
        try {
            reader = new BufferedReader(reader);
            StringBuilder builder = new StringBuilder();
            char[] chars = new char[0x10000];
            int length;
            while((length = reader.read(chars)) != -1) {
                builder.append(chars, 0, length);
            }
            return builder.toString();
        } catch(IOException e) {
            throw new FileException(e);
        } finally {
            close(reader);
        }
    }

    /** Reads an entire file as a string. */
    public static String read(InputStream stream) {
        return read(new InputStreamReader(stream));
    }

    /** Reads an entire file as a string. */
    public static String read(URL url) {
        try {
            return read(url.openStream());
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /** Reads an entire file as a string. */
    public static String read(URI uri) {
        try {
            return read(uri.toURL());
        } catch(MalformedURLException e) {
            throw new FileException(e);
        }
    }

    /** Reads an entire file as a string. */
    public static String read(File file) {
        try {
            return read(new FileReader(file));
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /** Returns an iterator that reads each line from the file. */
    public static LineIterator readLines(Reader reader) {
        return new LineIterator(new BufferedReader(reader));
    }

    /** Returns an iterator that reads each line from the file. */
    public static LineIterator readLines(InputStream stream) {
        return readLines(new InputStreamReader(stream));
    }

    /** Returns an iterator that reads each line from the file. */
    public static LineIterator readLines(URL url) {
        try {
            return readLines(url.openStream());
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /** Returns an iterator that reads each line from the file. */
    public static LineIterator readLines(URI uri) {
        try {
            return readLines(uri.toURL());
        } catch(MalformedURLException e) {
            throw new FileException(e);
        }
    }

    /** Returns an iterator that reads each line from the file. */
    public static LineIterator readLines(File file) {
        try {
            return readLines(new FileReader(file));
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /** Writes a string to a file. */
    public static void write(File file, String text) {
        write(file, text, false);
    }

    /** Writes a string to a file. */
    public static void write(File file, String text, boolean append) {
        try {
            write(new FileWriter(file, append), text);
        } catch (IOException e) {
            throw new FileException(e);
        }
    }

    /** Writes a string to a file. */
    public static void write(Writer writer, String text) {
        try {
            writer.write(text);
        } catch (IOException e) {
            throw new FileException(e);
        } finally {
            close(writer);
        }
    }

    /**
     * Closes any number of streams, readers, writers or anything else Closeable,
     * while ignoring handles that are null and without throwing any exceptions.
     */
    public static void close(Closeable... closeables) {
        try {
            for(Closeable closeable: closeables) {
                if(closeable != null) closeable.close();
            }
        } catch(IOException e) {
            // It is probably not interesting to know that the file couldn't be closed
        }
    }

    /** Copies a file. */
    public static void copy(InputStream in, OutputStream out) {
        try {
            byte[] buffer = new byte[0x10000];
            int length;
            while((length = in.read(buffer)) != -1) {
                out.write(buffer, 0, length);
            }
        } catch(IOException e) {
            throw new FileException(e);
        } finally {
            close(in, out);
        }
    }

    /** Copies a file. */
    public static void copy(File in, File out) {
        try {
            copy(new FileInputStream(in), new FileOutputStream(out));
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /** Copies a file. */
    public static void copy(URL url, File out) {
        try {
            copy(url.openStream(), new FileOutputStream(out));
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /** Copies a file. */
    public static void copy(URI uri, File out) {
        try {
            copy(uri.toURL(), out);
        } catch(IOException e) {
            throw new FileException(e);
        }
    }

    /**
     * Iterates over each line. It cuts off the line breaks. It can be used directly in a foreach.
     * You can get an array or a list out with toArray() and toList() respectively.
     * It automatically closes the file when all lines have been read.
     * Note that if you don't iterate to the end, it won't close the file until it is finalized.
     */
    public static final class LineIterator implements Iterator<String>, Iterable<String>, Closeable {
        private BufferedReader reader;
        private String nextLine;

        private LineIterator(BufferedReader reader) {
            this.reader = reader;
            this.nextLine = "";
            next();
        }

        /** Returns true if the end of the file has not been reached. */
        public boolean hasNext() {
            return nextLine != null;
        }

        /** Returns the next line if the end of the file has not been reached. */
        public String next() {
            if(nextLine == null) throw new NoSuchElementException();
            String line = nextLine;
            try {
                nextLine = reader.readLine();
                if(nextLine == null) {
                    close();
                }
            } catch (IOException e) {
                throw new FileException(e);
            }
            return line;
        }

        /** Not supported. */
        public void remove() {
            throw new UnsupportedOperationException();
        }

        /** This is a convenience method so it can be used directly in a foreach - it just returns this object. */
        public Iterator<String> iterator() {
            return this;
        }

        /** Returns the remaining lines as a list. */
        public List<String> toList() {
            ArrayList<String> list = new ArrayList<String>();
            for(String line: this) {
                list.add(line);
            }
            return list;
        }

        /** Returns the remaining lines as an array. */
        public String[] toArray() {
            List<String> list = toList();
            return list.toArray(new String[list.size()]);
        }

        /** This will automatically be called if you iterate through all the elements. */
        public void close() {
            if(reader != null) {
                reader = null;
                nextLine = null;
                Files.close(reader);
            }
        }
    }
}
