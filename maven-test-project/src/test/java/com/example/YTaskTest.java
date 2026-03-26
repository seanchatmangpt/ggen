package com.example;

import com.example.entity.YTask;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit test for generated YTask entity.
 */
public class YTaskTest {

    @Test
    public void testYTaskCreation() {
        YTask task = new YTask("task-001", "Approve Request", "net-001", "ATOMIC");

        assertNotNull(task);
        assertEquals("task-001", task.getTaskId());
        assertEquals("Approve Request", task.getTaskName());
        assertEquals("net-001", task.getNetId());
        assertEquals("ATOMIC", task.getTaskType());
        assertTrue(task.getIsEnabled());
    }

    @Test
    public void testYTaskSettersGetters() {
        YTask task = new YTask();
        task.setTaskId("task-002");
        task.setTaskName("Review Document");
        task.setNetId("net-002");
        task.setTaskType("COMPOSITE");
        task.setIsEnabled(false);
        task.setVersion(1);

        assertEquals("task-002", task.getTaskId());
        assertEquals("Review Document", task.getTaskName());
        assertEquals("net-002", task.getNetId());
        assertEquals("COMPOSITE", task.getTaskType());
        assertFalse(task.getIsEnabled());
        assertEquals(1, task.getVersion());
    }

    @Test
    public void testYTaskEqualsAndHashCode() {
        YTask task1 = new YTask("task-003", "Send Email", "net-003", "ATOMIC");
        YTask task2 = new YTask("task-003", "Send Email", "net-003", "ATOMIC");
        YTask task3 = new YTask("task-004", "Log Activity", "net-004", "ATOMIC");

        assertEquals(task1, task2);
        assertNotEquals(task1, task3);
        assertEquals(task1.hashCode(), task2.hashCode());
    }

    @Test
    public void testYTaskToString() {
        YTask task = new YTask("task-005", "Process Order", "net-005", "MULTIPLE_INSTANCE");
        String str = task.toString();

        assertTrue(str.contains("task-005"));
        assertTrue(str.contains("Process Order"));
        assertTrue(str.contains("net-005"));
        assertTrue(str.contains("MULTIPLE_INSTANCE"));
    }
}
