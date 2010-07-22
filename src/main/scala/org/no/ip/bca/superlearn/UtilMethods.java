package org.no.ip.bca.superlearn;

import static java.lang.Math.exp;

class UtilMethods {
    /**
     * Given the various visible and invisible states compute the delta and add
     * it to {@code m}.
     * 
     * @param v1
     *            Visible state 1
     * @param h1
     *            Hidden state 1
     * @param v2
     *            Visible state 2
     * @param h2
     *            Hidden state 2
     * @param m
     *            Matrix to add the delta to
     */
    public static void explode(final double[] v1, final double[] h1, final double[] v2, final double[] h2,
            final double[] m) {
        assert v1.length > 0;
        assert h1.length > 0;
        assert v1.length == v2.length;
        assert h1.length == h2.length;
        assert m.length >= v1.length * h1.length;
        
        int i = 0;
        for (int y = 0; y < h1.length; y++) {
            for (int x = 0; x < v1.length; x++) {
                m[i++] += v1[x] * h1[y] - v2[x] * h2[y];
            }
        }
    }
    
    /**
     * Multiplies vector {@code v} against matrix {@code m}, then "flattens" the
     * result to either 0.0 or 1.0 and assigns it to {@code out}.
     * <p/>
     * Assumes:
     * <ul>
     * <li>{@code v.length >= w}</li>
     * <li>{@code out.length >= h}</li>
     * </ul>
     * 
     * @param v
     * @param m
     * @param w
     *            Width of matrix m
     * @param h
     *            Height of matrix m
     * @param random
     * @param out
     */
    public static void mult(final double[] v, final double[] m, final int w, final int h, final double[] bias,
            final FastRandom random, final double[] out) {
        assert w > 0;
        assert h > 0;
        assert v.length >= w;
        assert m.length >= w * h;
        assert out.length >= h;
        
        int i = 0;
        for (int y = 0; y < h; y++) {
            double sum = 0.0;
            for (int x = 0; x < w; x++) {
                sum += v[x] * m[i++];
            }
            out[y] = 1.0 / (1.0 + exp(-sum - bias[y])) >= random.nextDouble() ? 1.0 : 0.0;
        }
    }
    
    /**
     * Converts a binary byte array to a double array of 0.0 and 1.0s. Any byte
     * == 0 is converted to a 0.0, and any byte != 0 is converted to a 1.0.
     * <p/>
     * Assumes:
     * <ul>
     * <li>{@code src.length <= dest.length}</li>
     * </ul>
     * 
     * @param src
     *            The input byte array.
     * @param dest
     *            The output array to write to.
     */
    public static void toBinaryDoubleArray(final byte[] src, final double[] dest) {
        assert src.length <= dest.length;
        
        for (int i = 0; i < src.length; i++) {
            if (src[i] != 0) {
                dest[i] = 1.0;
            }
        }
    }
    
    /**
     * Transposes {@code matrix} into {@code transpose}.
     * <p/>
     * Assumes:
     * <ul>
     * <li>{@code w} and {@code h} is the width and height of {@code matrix}</li>
     * <li>{@code transpose.length >= w * h}</li>
     * </ul>
     * 
     * @param matrix
     * @param w
     * @param h
     * @param transpose
     */
    public static void transpose(final double[] matrix, final int w, final int h, final double[] transpose) {
        assert w > 0;
        assert h > 0;
        assert matrix.length >= w * h;
        assert transpose.length >= w * h;
        
        int m = 0;
        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                transpose[x * h + y] = matrix[m++];
            }
        }
    }
    
    public static void sum(final double[] a, final double[] b) {
        for(int i = 0; i < a.length; i++) {
            b[i] += a[i];
        }
    }
    
    public static double mult(final double[] a, final double normalization, final double[] b) {
        for (int i = 0; i < a.length; i++) {
            a[i] = a[i] * normalization + b[i];
        }
        return 0.0;
    }
}
