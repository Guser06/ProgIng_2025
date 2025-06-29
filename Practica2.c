#include <stdio.h>  //Librería de entrada y salida de datos estandar//
#include <stdlib.h> //Librería estandar para generar numeros aleatorios en distribución uniforme//
#include <time.h>   //Librería para usar el tiempo como semilla de los numeros aleatorios y medir el tiempo de ejecución//
#include <math.h> // Necesario para funciones como sqrt, pow, HUGE_VALF, floorf, ceilf, isnan, isinf, fabs, log, exp

#define MAX_LIMIT 100.0f
#define MIN_LIMIT -100.0f

//Generar un numero aleatorio//
float aleatorios(float mini, float maxi) {
    return mini + (float)rand() / RAND_MAX * (maxi - mini);
}

//Generar datos aleatoriamente en un intervalo//
int opcion1(int *cant, float *mini, float *maxi) {
    printf("Ingrese la cant. de num. aleatorios (entero): ");
    scanf("%d", cant);
    if (*cant <= 0) {
        printf("La cant. debe ser > 0.\n");
        return 0;
    }

    do {
        printf("Ingrese el val. min. (Entre %.2f y %.2f): ", MIN_LIMIT, MAX_LIMIT);
        scanf("%f", mini);
        if (*mini < MIN_LIMIT || *mini > MAX_LIMIT) {
            printf("Valor minimo fuera del rango permitido (%.2f a %.2f).\n", MIN_LIMIT, MAX_LIMIT);
        }
    } while (*mini < MIN_LIMIT || *mini > MAX_LIMIT);

    do {
        printf("Ingrese el val. max. (Entre %.2f y %.2f): ", MIN_LIMIT, MAX_LIMIT);
        scanf("%f", maxi);
        if (*maxi < MIN_LIMIT || *maxi > MAX_LIMIT) {
            printf("Valor maximo fuera del rango permitido (%.2f a %.2f)..\n", MIN_LIMIT, MAX_LIMIT);
        }
    } while (*maxi < MIN_LIMIT || *maxi > MAX_LIMIT);

    if (*mini > *maxi) {
        printf("El val. min. no puede ser > al val. max.\n");
        return 0;
    }
    return 1;
}

//Solicitar la cantidad de datos a ingresar//
int numeros(int *cant) {
    printf("Ingrese la cant. de num. a introducir (entero): ");
    scanf("%d", cant);
    if (*cant <= 0) {
        printf("La cant. debe ser > 0.\n");
        return 0;
    }
    return 1;
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

//Imprimir los numeros generados aleatoriamente//
float* ImprimirA(int cant, float mini, float maxi) {
    float *nume = (float *)malloc(cant * sizeof(float));
    //Caso de error//
    if (!nume) {
        printf("Error: No se pudo asignar mem.\n");
        return NULL;
    }
    srand(time(NULL));
    printf("Numeros decimales generados:\n");
    for (int i = 0; i < cant; i++) {
        nume[i] = aleatorios(mini, maxi);
        printf("%.5f \n", nume[i]);  
    }
    printf("\n");
    return nume;
}

//Captura manual de datos//
float* opcion2(int cant) {
    float *nume = (float *)malloc(cant * sizeof(float));
    if (!nume) {
        printf("Error: No se pudo asignar mem.\n");
        return NULL;
    }
    printf("Ingrese los %d numeros decimales (Entre %.2f y %.2f):\n", cant, MIN_LIMIT, MAX_LIMIT);
    for (int i = 0; i < cant; i++) {
        do {
            printf("Numero %d: ", i + 1);
            scanf("%f", &nume[i]);
            if (nume[i] < MIN_LIMIT || nume[i] > MAX_LIMIT) {
                printf("Numero fuera del rango permitido (%.2f a %.2f). Intente de nuevo.\n", MIN_LIMIT, MAX_LIMIT);
            }
        } while (nume[i] < MIN_LIMIT || nume[i] > MAX_LIMIT);
    }
    printf("\n");
    return nume;
}

// Se corrigió la función historial para usar roundf en lugar de solo (int)
int* historial(float *arre, int cant, int min, int max) {
    if (min > max) return NULL;
    int tamano = max - min + 1;
    if (tamano <= 0) return NULL; // Manejar el caso donde el rango es 0 o negativo
    int *hist = (int *)calloc(tamano, sizeof(int));
    if (!hist) return NULL;
    for (int i = 0; i < cant; i++) {
        // Redondear al entero más cercano para determinar el intervalo
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

//Mediana//
float mediana(float *arre, int cant){
    if (cant == 0) return 0.0f;
    return (cant % 2 == 0) ? (arre[cant / 2 - 1] + arre[cant / 2]) / 2.0f : arre[cant / 2];
}

// Función media geometrica mejorada utilizando logaritmos
double mediag(float *arre, int cant, int *out_num_negativos, int *out_contiene_cero){
    *out_num_negativos = 0; 
    *out_contiene_cero = 0; 

    if (cant == 0) {
        return NAN; // Usar NAN para indicar que la media no es aplicable para un arreglo vacío
    }

    double sum_logs = 0.0;
    for (int i = 0; i < cant; i++) {
        if (arre[i] == 0.0f) {
            *out_contiene_cero = 1; 
            return 0.0; // La media geométrica es 0 si hay un cero
        }
        if (arre[i] < 0.0f) {
            (*out_num_negativos)++;
        }
        // Usar log del valor absoluto para la suma de logaritmos
        sum_logs += log(fabs((double)arre[i])); 
    }
    
    double resultado = exp(sum_logs / cant);

    // Devolver el valor real; out_num_negativos se usa para señalar 'i' en la impresión.
    return resultado;
}

//Moda de los datos//
float moda(int *hist, int min, int max){
    if (!hist || (max - min + 1) <= 0) return HUGE_VALF; // Usar HUGE_VALF para indicar que no hay moda válida

    int max_f = 0;
    float moda_val_float = HUGE_VALF; 

    for (int i = 0; i <= (max - min); i++) {
        if (hist[i] > max_f) {
            max_f = hist[i];
            moda_val_float = (float)(min + i);
        }
    }
    
    if (max_f == 0) { // Si todas las frecuencias son 0, significa que no hay elementos en el histograma.
        return HUGE_VALF; 
    }

    return moda_val_float;
}

//Función general para obtener cuantiles//
float obtener_cuantil(float *arre, int cant, int i, int divisor) {
    if (!arre || cant <= 0 || i <= 0 || i > divisor) return 0.0f;
    double posicion = (double)i * cant / divisor;
    int indice_0_base = (int)ceil(posicion) - 1;
    if (indice_0_base < 0) indice_0_base = 0;
    if (indice_0_base >= cant) indice_0_base = cant - 1;
    return arre[indice_0_base];
}

//Función para mostrar los cuantiles de manera ordenada//
void imprimir_cuantiles(float *arre, int cant) {
    if (cant <= 0) {
        printf("\nNo se pueden calcular cuantiles para un arreglo vacio.\n");
        return;
    }
    printf("\n--- Cuartiles ---\n");
    printf("Cuartil 1 (Q1): %.8f\n", obtener_cuantil(arre, cant, 1, 4));
    printf("Cuartil 2 (Q2): %.8f\n", mediana(arre, cant));
    printf("Cuartil 3 (Q3): %.8f\n", obtener_cuantil(arre, cant, 3, 4));
    printf("\n--- Deciles ---\n");
    for (int i = 1; i <= 10; i++) {
        printf("Decil %d (D%d%s): %.8f\n", i, i, (i == 5 ? " - Mediana" : ""), (i == 5 ? mediana(arre, cant) : obtener_cuantil(arre, cant, i, 10)));
    }
    printf("\n--- Percentiles ---\n");
    for (int i = 10; i <= 100; i += 10) {
        printf("Percentil %d (P%d%s): %.8f\n", i, i, (i == 50 ? " " : ""), (i == 50 ? mediana(arre, cant) : obtener_cuantil(arre, cant, i, 100)));
    }
}

//Funcion para obtener los momentos de diferentes ordenes//
void momento(float *arre, int cant){
    if (cant <= 0) {
        printf("No se pueden calcular momentos para un arreglo vacio.\n");
        return;
    }

    double media_calc = (double)mediaa(arre, cant);

    double sum_diff_pow2 = 0.0;
    double sum_diff_pow3 = 0.0;
    double sum_diff_pow4 = 0.0;

    double m[4] = {0.0};    

    //Respecto al origen//
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

    printf("\n--- Momentos (respecto al origen) ---\n");
    printf("Momento 0: %.8f\n", 1.0);
    printf("Momento 1: %.8f\n", a[0]);
    printf("Momento 2: %.8f\n", a[1]);
    printf("Momento 3: %.8f\n", a[2]);
    printf("Momento 4: %.8f\n", a[3]);

    //Respecto a la media//
    double c[5];
    c[0] = 1.0;
    c[1] = 0.0; 
    c[2] = sum_diff_pow2 / cant; 
    c[3] = sum_diff_pow3 / cant; 
    c[4] = sum_diff_pow4 / cant; 

    printf("\n--- Momentos Centrales (respecto a la media) ---\n");
    printf("Momento 0: %.8f\n", c[0]);
    printf("Momento 1: %.8f\n", c[1]);
    printf("Momento 2: %.8f\n", c[2]);
    printf("Momento 3: %.8f\n", c[3]);
    printf("Momento 4: %.8f\n", c[4]);

    float varianza_muestral = (cant > 1) ? (float)sum_diff_pow2 / (cant) : 0.0f; // Varianza poblacional
    printf("\nVarianza (poblacional): %.8f\n", varianza_muestral);
    
    // Si necesitas la varianza muestral (dividida por n-1):
    float varianza_muestral_n_minus_1 = (cant > 1) ? (float)sum_diff_pow2 / (cant - 1) : 0.0f;
    printf("Varianza (muestral, n-1): %.8f\n", varianza_muestral_n_minus_1);
    
    float desviacion_estandar = sqrt(varianza_muestral); // Desviación estándar poblacional
    printf("Desviacion estandar (poblacional): %.8f\n", desviacion_estandar);
    
    float desviacion_estandar_n_minus_1 = sqrt(varianza_muestral_n_minus_1); // Desviación estándar muestral
    printf("Desviacion estandar (muestral, n-1): %.8f\n", desviacion_estandar_n_minus_1);


    double desviacion_estandar_d = (double)desviacion_estandar;

    printf("\n--- Momentos Estandar ---\n");
    printf("Momento Estandar (k=1): %.8f\n", 0.0);

    double momento_estandar_2;
    if (desviacion_estandar_d != 0.0) {
        momento_estandar_2 = c[2] / pow(desviacion_estandar_d, 2);
    } else {
        momento_estandar_2 = 0.0; 
    }
    printf("Momento Estandar (k=2): %.8f\n", momento_estandar_2);

    double momento_estandar_3;
    if (desviacion_estandar_d != 0.0) {
        momento_estandar_3 = c[3] / pow(desviacion_estandar_d, 3);
    } else {
        momento_estandar_3 = 0.0;
    }
    printf("Momento Estandar (k=3 - Asimetria): %.8f\n", momento_estandar_3);

    double momento_estandar_4;
    if (desviacion_estandar_d != 0.0) {
        momento_estandar_4 = c[4] / pow(desviacion_estandar_d, 4); 
    } else {
        momento_estandar_4 = 0.0;
    }
    // La curtosis es directamente el momento estandar 4
    printf("Momento Estandar (k=4): %.8f\n", momento_estandar_4);
    printf("Curtosis: %.8f\n", momento_estandar_4);
}

//Mostar un resumen de los resultados//
int resumen(float re_may, float re_men, int *hist, int mini_hist, int maxi_hist, int cant, float *arre) {
    printf("\n--- Resumen ---\n");
    printf("Numeros ordenados:\n");
    for (int i = 0; i < cant; i++) printf("%.5f \n", arre[i]);
    printf("\nEl numero mayor: %.8f\n", re_may);
    printf("El numero menor: %.8f\n", re_men);

    float media_calculada = mediaa(arre, cant);
    printf("Media: %.8f\n", media_calculada);
    printf("Mediana: %.8f\n", mediana(arre, cant));

    int num_negativos_en_mediag;
    int contiene_cero_en_mediag; // Nueva variable para el control de ceros
    double media_geom_val = mediag(arre, cant, &num_negativos_en_mediag, &contiene_cero_en_mediag); // Pasar la nueva variable

    if (contiene_cero_en_mediag) {
        printf("Media Geom.: 0.00000000\n"); // Imprimir 0 si se encontró un cero
    } else if (isnan(media_geom_val)) {
        printf("Media Geom.: N/A (no aplicable)\n"); // Si es NAN
    } else if (num_negativos_en_mediag % 2 != 0) {
        printf("Media Geom.: %.8fi\n", media_geom_val); 
    } else {
        printf("Media Geom.: %.8f\n", media_geom_val);
    }

    float moda_val = moda(hist, mini_hist, maxi_hist);
    if (moda_val == HUGE_VALF) {
        printf("Moda: N/A\n"); 
    } else {
        printf("Moda: %.1f\n", moda_val); 
    }

    imprimir_cuantiles(arre, cant);
    momento(arre, cant); 

    if (hist) {
        printf("\n--- Histograma (partes enteras) ---\n");
        for (int i = 0; i <= (maxi_hist - mini_hist); i++) {
            // Solo imprimir intervalos que tienen elementos
            if (hist[i] > 0) printf("h(%d): %d\n", mini_hist + i, hist[i]);
        }
        return 1;
    } else {
        printf("\nNo se pudo generar el histograma.\n");
        return 0;
    }
}

int main() {
    //Declarar variables//
    int opci, cant;
    float mini, maxi, re_may, re_men;
    float *arr_num = NULL;
    int *hist_res = NULL;
    int mini_hist = 0, maxi_hist = 0;

    //Solicitar al usuario elegir entre captura manual y generacion aleatoria de valores//
    printf("Elija una opcion:\n");
    printf("1. Numeros aleatorios\n");
    printf("2. Introducir numeros manualmente\n");
    printf("Opcion: ");
    scanf("%d", &opci);
    printf("\n");

    if (opci == 1) {
        if (!opcion1(&cant, &mini, &maxi)) return 1;
        printf("\n");
        arr_num = ImprimirA(cant, mini, maxi);
    } else if (opci == 2) {
        if (!numeros(&cant)) return 1;
        printf("\n");
        arr_num = opcion2(cant);
    } else {
        printf("Opcion invalida.\n");
        return 1;
    }
    clock_t tStart = clock();
    //Casos de error//
    if (!arr_num || cant <= 0) {
        if (arr_num) free(arr_num);
        return 1;
    }

    if (!ordenamiento(arr_num, cant)) {
        printf("Error al ordenar el arreglo.\n");
        free(arr_num);
        return 1;
    }

    re_men = arr_num[0];
    re_may = arr_num[cant - 1];
    
    // Ajustar los límites del histograma para que no excedan MIN_LIMIT/MAX_LIMIT
    mini_hist = (int)floorf(re_men); // Usar floorf para asegurar el rango correcto
    maxi_hist = (int)ceilf(re_may);  // Usar ceilf para asegurar el rango correcto

    if (mini_hist < MIN_LIMIT) mini_hist = (int)MIN_LIMIT;
    if (maxi_hist > MAX_LIMIT) maxi_hist = (int)MAX_LIMIT;
    if (mini_hist > maxi_hist) maxi_hist = mini_hist; 

    hist_res = historial(arr_num, cant, mini_hist, maxi_hist);
    resumen(re_may, re_men, hist_res, mini_hist, maxi_hist, cant, arr_num);

    free(arr_num);
    if (hist_res) {
        free(hist_res);
    }
    printf("Tiempo de ejecucion: %.2fs\n", (double)(clock() - tStart)/CLOCKS_PER_SEC);
    return 0;
}