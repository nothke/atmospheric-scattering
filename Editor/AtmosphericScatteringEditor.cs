using UnityEditor;
using UnityEngine;

[CustomEditor(typeof(AtmosphericScattering))]
class AtmosphericScatteringEditor : Editor
{
    public override void OnInspectorGUI()
    {
        //serializedObject.Update();

        AtmosphericScattering a = (AtmosphericScattering)target;

        string errors = a.Validate().TrimEnd();
        if (errors != "")
            EditorGUILayout.HelpBox(errors, MessageType.Error);

        DrawDefaultInspector();

        if (GUILayout.Button("Update LookUp Tables"))
            a.CalculateLightLUTs();

        //serializedObject.ApplyModifiedProperties();
    }
}