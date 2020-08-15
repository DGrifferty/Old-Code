import math

def weightedmean(x_values, y_values, error_in_y_values, n):
    for x in x_values:

        indices = []
        p, top, bottom, n_new = -1, 0, 0, 0

        if x_values.count(x) >= 2:

            for element in x_values:
                p += 1
                if element == x:
                    indices.append(p)

            for i in indices:
                top += y_values[i] / (error_in_y_values[i] ** 2)
                bottom += 1 / (error_in_y_values[i] ** 2)
                n_new += n[i]

            for i in indices:

                if indices.index(i) == 0:

                    y_values[i] = top / bottom
                    error_in_y_values[i] = math.sqrt(1 / bottom)
                    n[i] = n_new

                elif indices.index(i) != 0:

                    del y_values[i]
                    del error_in_y_values[i]
                    del x_values[i]
                    del n[i]

                    for t in indices:
                        if t > i:
                            indices[indices.index(t)] -= 1
