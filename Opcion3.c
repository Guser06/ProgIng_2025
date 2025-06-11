#include <stdio.h>  //Librería de entrada y salida de datos estandar//
#include <stdlib.h> //Librería estandar para generar numeros aleatorios en distribución uniforme//
#include <time.h>   //Librería para usar el tiempo como semilla de los numeros aleatorios//
#include <math.h>   //Librería con funciones matemáticas utilizadas como potencias y raices//

#define NUM_EJECUCIONES_OPCION3 20
#define MAX_CANT_ALEATORIOS 10000

//Generar un numero aleatorio//
float aleatorios(float mini, float maxi) {
    if (mini >= maxi) return mini;
    return mini + (float)rand() / RAND_MAX * (maxi - mini);
}

//Ordenamiento de muestras mediante Quick Sort//
int ordenamiento(float *arre, int cant) {
    if (!arre || cant <= 0) return 0;

    int stack[100 * 2];
    int top = -1;

    stack[++top] = 0;
    stack[++top] = cant - 1;

    while (top >= 0) {
        int alto = stack[top--];
        int bajo = stack[top--];

        float pivote = arre[alto];
        int i = (bajo - 1);

        for (int j = bajo; j <= alto - 1; j++) {
            if (arre[j] < pivote) {
                i++;
                float temp = arre[i];
                arre[i] = arre[j];
                arre[j] = temp;
            }
        }
        float temp = arre[i + 1];
        arre[i + 1] = arre[alto];
        arre[alto] = temp;

        int pi = (i + 1);

        if (pi - 1 > bajo) {
            stack[++top] = bajo;
            stack[++top] = pi - 1;
        }

        if (pi + 1 < alto) {
            stack[++top] = pi + 1;
            stack[++top] = alto;
        }
    }
    return 1;
}

//Generar una cantidad n de numeros aleatorios en un intervalo//
float* GenerarAleatorios(int cant, float mini, float maxi) {
    float *nume = (float *)malloc(cant * sizeof(float));
    if (!nume) {
        return NULL;
    }
    for (int i = 0; i < cant; i++) {
        nume[i] = aleatorios(mini, maxi);
    }
    return nume;
}

//Crear el histograma de los datos ordenados//
int* historial(float *arre, int cant, int min, int max) {
    if (min > max) return NULL;

    int tamano = max - min + 1;
    if (tamano <= 0) return NULL;

    int *hist = (int *)calloc(tamano, sizeof(int));
    if (!hist) return NULL;

    for (int i = 0; i < cant; i++) {
        int intervalo = (int)roundf(arre[i]);
        if (intervalo >= min && intervalo <= max) {
            int indice = intervalo - min;
            hist[indice]++;
        }
    }
    return hist;
}

//Media aritmetica//
float mediaa(float *arre, int cant){
    float media = 0;
    if (cant == 0) return 0.0f;
    for (int i = 0; i < cant; i++) {
        media += arre[i];
    }
    return media / cant;
}

//Mediana de los datos ordenados//
float mediana(float *arre, int cant){
    if (cant == 0) return 0.0f;
    if (cant % 2 == 0) {
        return (arre[cant / 2 - 1] + arre[cant / 2]) / 2.0f;
    } else {
        return arre[cant / 2];
    }
}

//Media geometrica//
double mediag(float *arre, int cant, int *out_num_negativos, int *out_contiene_cero){
    *out_num_negativos = 0;
    *out_contiene_cero = 0;

    if (cant == 0) {
        return NAN;
    }

    double sum_logs = 0.0;
    for (int i = 0; i < cant; i++) {
        if (arre[i] == 0.0f) {
            *out_contiene_cero = 1;
            return 0.0;
        }
        if (arre[i] < 0.0f) {
            (*out_num_negativos)++;
        }
        sum_logs += log(fabs((double)arre[i]));
    }

    double resultado = exp(sum_logs / cant);
    return resultado;
}

//Moda de las muestras//
float moda(int *hist, int min, int max){
    if (!hist || (max - min + 1) <= 0) return HUGE_VALF;

    int max_f = 0;
    float moda_val_float = HUGE_VALF;

    for (int i = 0; i <= (max - min); i++) {
        if (hist[i] > max_f) {
            max_f = hist[i];
            moda_val_float = (float)(min + i);
        }
    }

    if (max_f == 0) {
        return HUGE_VALF;
    }

    return moda_val_float;
}

//Funcion para obtener cuantiles de diferentes intervalos//
float obtener_cuantil(float *arre, int cant, int i, int divisor) {
    if (!arre || cant <= 0 || i <= 0 || i > divisor) return 0.0f;
    double posicion = (double)i * cant / divisor;
    int indice_0_base = (int)ceil(posicion) - 1;
    if (indice_0_base < 0) indice_0_base = 0;
    if (indice_0_base >= cant) indice_0_base = cant - 1;
    return arre[indice_0_base];
}

//Funcion para calcular los momentos respecto al origen 0 o a la media//
void calcular_momentos(float *arre, int cant,
             double *out_momento_origen_1, double *out_momento_origen_2,
             double *out_momento_origen_3, double *out_momento_origen_4,
             double *out_momento_central_2, double *out_momento_central_3,
             double *out_momento_central_4, double *out_varianza_muestral,
             double *out_desviacion_estandar_muestral, double *out_momento_estandar_2,
             double *out_momento_estandar_3, double *out_momento_estandar_4,
             double *out_curtosis_final){
    //Momentos de orden 0 siempre son iguales a 0//
    if (cant <= 0) {
        *out_momento_origen_1 = 0.0; *out_momento_origen_2 = 0.0; *out_momento_origen_3 = 0.0; *out_momento_origen_4 = 0.0;
        *out_momento_central_2 = 0.0; *out_momento_central_3 = 0.0; *out_momento_central_4 = 0.0;
        *out_varianza_muestral = 0.0; *out_desviacion_estandar_muestral = 0.0;
        *out_momento_estandar_2 = 0.0; *out_momento_estandar_3 = 0.0; *out_momento_estandar_4 = 0.0;
        *out_curtosis_final = 0.0;
        return;
    }

    double media_calc = (double)mediaa(arre, cant);

    double sum_diff_pow2 = 0.0;
    double sum_diff_pow3 = 0.0;
    double sum_diff_pow4 = 0.0;

    double m[4] = {0.0};

    //Calcular primeros 4 momentos//
    for (int i = 0; i < cant; i++) {
        double valor_actual = (double)arre[i];
        double diff = valor_actual - media_calc;

        m[0] += valor_actual;
        m[1] += pow(valor_actual, 2);
        m[2] += pow(valor_actual, 3);
        m[3] += pow(valor_actual, 4);

        sum_diff_pow2 += pow(diff, 2);
        sum_diff_pow3 += pow(diff, 3);
        sum_diff_pow4 += pow(diff, 4);
    }

    double a[4];
    a[0] = m[0] / cant;
    a[1] = m[1] / cant;
    a[2] = m[2] / cant;
    a[3] = m[3] / cant;

    *out_momento_origen_1 = a[0];
    *out_momento_origen_2 = a[1];
    *out_momento_origen_3 = a[2];
    *out_momento_origen_4 = a[3];

    double c[5];
    c[0] = 1.0;
    c[1] = 0.0;
    c[2] = sum_diff_pow2 / cant;
    c[3] = sum_diff_pow3 / cant;
    c[4] = sum_diff_pow4 / cant;

    *out_momento_central_2 = c[2];
    *out_momento_central_3 = c[3];
    *out_momento_central_4 = c[4];

    float varianza_muestral_local = (cant > 1) ? (float)sum_diff_pow2 / (cant - 1) : 0.0f;
    *out_varianza_muestral = varianza_muestral_local;
    float desviacion_estandar_local = sqrt(varianza_muestral_local);
    *out_desviacion_estandar_muestral = desviacion_estandar_local;

    double desviacion_estandar_d = (double)desviacion_estandar_local;

    double momento_estandar_2_local;
    if (desviacion_estandar_d != 0.0) {
        momento_estandar_2_local = c[2] / pow(desviacion_estandar_d, 2);
    } else {
        momento_estandar_2_local = 0.0;
    }
    *out_momento_estandar_2 = momento_estandar_2_local;

    double momento_estandar_3_local;
    if (desviacion_estandar_d != 0.0) {
        momento_estandar_3_local = c[3] / pow(desviacion_estandar_d, 3);
    } else {
        momento_estandar_3_local = 0.0;
    }
    *out_momento_estandar_3 = momento_estandar_3_local;

    double momento_estandar_4_local;
    if (desviacion_estandar_d != 0.0) {
        momento_estandar_4_local = c[4] / pow(desviacion_estandar_d, 4);
    } else {
        momento_estandar_4_local = 0.0;
    }
    *out_momento_estandar_4 = momento_estandar_4_local;

    *out_curtosis_final = momento_estandar_4_local;
}


//Funcion para escribir los resultados obtenidos en un archivo CSV//
void resumen_a_csv(FILE *fp, float re_may, float re_men, float mini_range_gen, float maxi_range_gen,
                       int *hist, int mini_hist, int maxi_hist, int cant, float *arre, int iteracion, int is_header_row) {

    double mom_origen_1, mom_origen_2, mom_origen_3, mom_origen_4;
    double mom_central_2, mom_central_3, mom_central_4;
    double var_muestral, desv_estandar_muestral;
    double mom_estandar_2, mom_estandar_3, mom_estandar_4;
    double curtosis_val;

    //Calcular los diferentes momentos de las muestras//
    if (!is_header_row) {
        calcular_momentos(arre, cant, &mom_origen_1, &mom_origen_2, &mom_origen_3, &mom_origen_4,
                              &mom_central_2, &mom_central_3, &mom_central_4, &var_muestral,
                              &desv_estandar_muestral, &mom_estandar_2, &mom_estandar_3,
                              &mom_estandar_4, &curtosis_val);
    }

    //Nombrar columnas//
    if (is_header_row) {
        fprintf(fp, "Iteracion,Cantidad_Numeros,Rango_Min_Generado,Rango_Max_Generado,Maximo,Minimo,Media,Media_Geometrica,Mediana,Moda,"
                        "Momento_Origen_1,Momento_Origen_2,Momento_Origen_3,Momento_Origen_4,"
                        "Momento_Central_2,Momento_Central_3,Momento_Central_4,"
                        "Varianza_Muestral,Desviacion_Estandar_Muestral,"
                        "Momento_Estandar_2,Momento_Estandar_3,Momento_Estandar_4,Curtosis,"
                        "Cuartil_1,Cuartil_2,Cuartil_3,"
                        "Decil_1,Decil_2,Decil_3,Decil_4,Decil_5,Decil_6,Decil_7,Decil_8,Decil_9,Decil_10,"
                        "Percentil_10,Percentil_20,Percentil_30,Percentil_40,Percentil_50,Percentil_60,Percentil_70,Percentil_80,Percentil_90,Percentil_100\n");
    } else {
        //Nombrar columnas//
        fprintf(fp, "%d,%d,%.2f,%.2f,", iteracion, cant, mini_range_gen, maxi_range_gen);

        fprintf(fp, "%.6f,%.6f,%.6f,",
                         re_may, re_men, mediaa(arre, cant));


        int num_negativos_en_mediag;
        int contiene_cero;
        double media_geom_val = mediag(arre, cant, &num_negativos_en_mediag, &contiene_cero);

        //Impresiones para casos donde los valores son no numericos//
        if (contiene_cero) {
            fprintf(fp, "0.0,");
        } else if (isnan(media_geom_val)) {
            fprintf(fp, "N/A,");
        } else if (num_negativos_en_mediag % 2 != 0) {
            fprintf(fp, "%.6fi,", media_geom_val);
        } else {
            fprintf(fp, "%.6f,", media_geom_val);
        }

        //Imprimir la mediana en el archivo//
        fprintf(fp, "%.6f,", mediana(arre, cant));

        //Imprimir la moda en el archivo//
        float moda_val = moda(hist, mini_hist, maxi_hist);
        if (moda_val == HUGE_VALF) {
            fprintf(fp, "N/A,");
        } else {
            fprintf(fp, "%.6f,", moda_val);
        }

        //Impresiones de momentos//
        fprintf(fp, "%.6f,%.6f,%.6f,%.6f,", mom_origen_1, mom_origen_2, mom_origen_3, mom_origen_4);

        fprintf(fp, "%.6f,%.6f,%.6f,", mom_central_2, mom_central_3, mom_central_4);

        fprintf(fp, "%.6f,%.6f,", var_muestral, desv_estandar_muestral);

        fprintf(fp, "%.6f,%.6f,%.6f,%.6f,", mom_estandar_2, mom_estandar_3, mom_estandar_4, curtosis_val);

        //Impresiones de cuartiles//
        fprintf(fp, "%.6f,%.6f,%.6f,",
                         obtener_cuantil(arre, cant, 1, 4),
                         obtener_cuantil(arre, cant, 2, 4),
                         obtener_cuantil(arre, cant, 3, 4));

        for (int i = 1; i <= 10; i++) {
            fprintf(fp, "%.6f%s", obtener_cuantil(arre, cant, i, 10), (i == 10 ? "" : ","));
        }

        for (int i = 10; i <= 100; i += 10) {
            fprintf(fp, ",%.6f", obtener_cuantil(arre, cant, i, 100));
        }
        fprintf(fp, "\n");
    }
}

//Programa principal//
int main() {
    int cant;
    float mini_gen, maxi_gen, re_may, re_men;
    float *arr_num = NULL;
    int *hist_res = NULL;
    int mini_hist = 0, maxi_hist = 0;

    //Semilla de numeros aleatorios//
    srand(time(NULL));

    //Informar al usuario del archivo csv que se creará//
    printf("Generando archivo 'resultados.csv' con %d conjuntos de numeros aleatorios (rangos variables, multiplos de 100)...\n", NUM_EJECUCIONES_OPCION3);

    //Abrir el archivo "resultados.csv" en modo de escritura//
    FILE *fp = fopen("resultados.csv", "w");
    if (fp == NULL) {
        printf("Error al crear el archivo resultados.csv\n");
        return 1;
    }

    //Colocar las medidas estadisticas en el csv//
    resumen_a_csv(fp, 0, 0, 0.0f, 0.0f, NULL, 0, 0, 0, NULL, 0, 1);

    //Ejecutar el programa para los 20 casos solicitados//
    for (int i = 1; i <= NUM_EJECUCIONES_OPCION3; i++) {
        int factor = (rand() % (MAX_CANT_ALEATORIOS / 100)) + 1;
        cant = factor * 100;

        mini_gen = (float)(rand() % 201) - 100.0f;
        maxi_gen = (float)(rand() % 201) - 100.0f;

        if (mini_gen > maxi_gen) {
            float temp = mini_gen;
            mini_gen = maxi_gen;
            maxi_gen = temp;
        }

        if (fabs(maxi_gen - mini_gen) < 10.0f || (mini_gen >= 0 && maxi_gen >= 0) || (mini_gen <= 0 && maxi_gen <= 0)) {
            if (mini_gen >= 0) {
                mini_gen = -((float)rand() / RAND_MAX * 50.0f + 1.0f);
            }
            if (maxi_gen <= 0) {
                maxi_gen = ((float)rand() / RAND_MAX * 50.0f + 1.0f);
            }

            if (mini_gen > maxi_gen) {
                float temp = mini_gen;
                mini_gen = maxi_gen;
                maxi_gen = temp;
            }

            if (mini_gen < -100.0f) mini_gen = -100.0f;
            if (maxi_gen > 100.0f) maxi_gen = 100.0f;

            if (fabs(maxi_gen - mini_gen) < 1.0f) {
                mini_gen = -50.0f;
                maxi_gen = 50.0f;
            }
        }

        //Generar al azar los numeros requeridos//
        arr_num = GenerarAleatorios(cant, mini_gen, maxi_gen);

        //Caso de error al asignar memoria//
        if (!arr_num) {
            fprintf(fp, "%d,%d,%.2f,%.2f,Error: No se pudo asignar memoria.\n", i, cant, mini_gen, maxi_gen);
            continue;
        }

        //Caso de error al ordenar los numeros//
        if (!ordenamiento(arr_num, cant)) {
            fprintf(fp, "%d,%d,%.2f,%.2f,Error: No se pudo ordenar el arreglo.\n", i, cant, mini_gen, maxi_gen);
            free(arr_num);
            continue;
        }

        //Maximo y minimo valores generados
        re_men = arr_num[0];
        re_may = arr_num[cant - 1];

        mini_hist = (int)floorf(re_men);
        maxi_hist = (int)ceilf(re_may);

        if (mini_hist < -100) mini_hist = -100;
        if (maxi_hist > 100) maxi_hist = 100;
        if (mini_hist > maxi_hist) {
            maxi_hist = mini_hist;
        }

        //Crear histograma de resultados//
        hist_res = historial(arr_num, cant, mini_hist, maxi_hist);
        //Caso de error al crear histograma//
        if (!hist_res) {
            fprintf(fp, "%d,%d,%.2f,%.2f,Error: No se pudo generar el histograma.\n", i, cant, mini_gen, maxi_gen);
            free(arr_num);
            continue;
        }

        //Incorporar los resultados al csv//
        resumen_a_csv(fp, re_may, re_men, mini_gen, maxi_gen, hist_res, mini_hist, maxi_hist, cant, arr_num, i, 0);

        //Liberar memoria utilizada en el almacenamiento de las muestras//
        free(arr_num);
        free(hist_res);
        arr_num = NULL;
        hist_res = NULL;
    }

    //Cerrar el archivo//
    fclose(fp);
    printf("\nResultados guardados en 'resultados.csv'\n");

    //Finalizar el programa//
    return 0;
}