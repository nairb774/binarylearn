package org.no.ip.bca.superlearn;

class UtilMethods {
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
}
