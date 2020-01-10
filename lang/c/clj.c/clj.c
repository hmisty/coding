#include <jni.h>

/* src/clj.c:20: warning: ‘JNI_CreateJavaVM’ is deprecated (declared at /System/Library/Frameworks/JavaVM.framework/Headers/jni.h:1937)
 */

#define OK 0
#define ERROR_CREATE_JVM 1
#define CLASS_NOT_FOUND 2
#define METHOD_NOT_FOUND 3

int main() {
	JavaVM *jvm;       /* denotes a Java VM */
	JNIEnv *env;       /* pointer to native method interface */
	JavaVMInitArgs vm_args; /* JDK/JRE 6 VM initialization arguments */
	JavaVMOption options[1];
	options[0].optionString = "-Djava.class.path=/usr/lib/quicklojure/ext/clojure-1.5.1.jar";
	vm_args.version = JNI_VERSION_1_6;
	vm_args.nOptions = 1;
	vm_args.options = options;
	vm_args.ignoreUnrecognized = JNI_FALSE;
	/* load and initialize a Java VM, return a JNI interface
	 * pointer in env */
	int err = JNI_CreateJavaVM(&jvm, 
			(void**)&env, 
			&vm_args);
	if (err != JNI_OK) return ERROR_CREATE_JVM;

	/* invoke the clojure/main.main method using the JNI */
	jclass cls = (*env)->FindClass(env, "clojure/main");
	if (cls == NULL) return CLASS_NOT_FOUND;

	jmethodID mid = (*env)->GetStaticMethodID(env, cls, "main", "([Ljava/lang/String;)V");
	if (mid == NULL) return METHOD_NOT_FOUND;

	jobjectArray args = (*env)->NewObjectArray(env, 1, (*env)->FindClass(env, "java/lang/String"), NULL);
	jstring arg0 = (*env)->NewStringUTF(env, "-e '(+ 1 2)'");
	(*env)->SetObjectArrayElement(env, args, 0, arg0);
	(*env)->CallStaticVoidMethod(env, cls, mid, args);

	/* We are done. */
	(*jvm)->DestroyJavaVM(jvm);
	return OK;
}
