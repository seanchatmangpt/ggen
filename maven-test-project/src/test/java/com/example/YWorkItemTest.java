package com.example;

import com.example.entity.YWorkItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit test for generated YWorkItem entity.
 */
public class YWorkItemTest {

    @Test
    public void testYWorkItemCreation() {
        YWorkItem item = new YWorkItem("wi-001", "task-01", "case-001", "ENABLED");

        assertNotNull(item);
        assertEquals("wi-001", item.getWorkItemId());
        assertEquals("task-01", item.getTaskId());
        assertEquals("case-001", item.getCaseId());
        assertEquals("ENABLED", item.getStatus());
    }

    @Test
    public void testYWorkItemSettersGetters() {
        YWorkItem item = new YWorkItem();
        item.setWorkItemId("wi-002");
        item.setTaskId("task-02");
        item.setCaseId("case-002");
        item.setStatus("EXECUTING");
        item.setResourceAssigned("user-123");
        item.setVersion(1);

        assertEquals("wi-002", item.getWorkItemId());
        assertEquals("task-02", item.getTaskId());
        assertEquals("case-002", item.getCaseId());
        assertEquals("EXECUTING", item.getStatus());
        assertEquals("user-123", item.getResourceAssigned());
        assertEquals(1, item.getVersion());
    }

    @Test
    public void testYWorkItemEqualsAndHashCode() {
        YWorkItem item1 = new YWorkItem("wi-003", "task-03", "case-003", "COMPLETED");
        YWorkItem item2 = new YWorkItem("wi-003", "task-03", "case-003", "COMPLETED");
        YWorkItem item3 = new YWorkItem("wi-004", "task-04", "case-004", "ENABLED");

        assertEquals(item1, item2);
        assertNotEquals(item1, item3);
        assertEquals(item1.hashCode(), item2.hashCode());
    }

    @Test
    public void testYWorkItemToString() {
        YWorkItem item = new YWorkItem("wi-005", "task-05", "case-005", "SUSPENDED");
        String str = item.toString();

        assertTrue(str.contains("wi-005"));
        assertTrue(str.contains("task-05"));
        assertTrue(str.contains("case-005"));
        assertTrue(str.contains("SUSPENDED"));
    }
}
