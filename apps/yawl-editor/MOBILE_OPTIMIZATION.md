# Mobile Optimization Guide

This guide covers the mobile optimization strategies implemented across the YAWL Editor platform.

## Overview

All pages and components are optimized for mobile-first responsive design, ensuring excellent user experience across all device sizes (mobile, tablet, desktop).

## Mobile-First Breakpoints

Using Tailwind CSS breakpoints:
- **Mobile**: < 640px
- **Tablet**: 640px - 1024px
- **Desktop**: > 1024px

## Responsive Components

### Text Scaling
All text components scale responsively:
```tsx
<h1 className="text-2xl md:text-3xl lg:text-4xl">Title</h1>
<p className="text-sm md:text-base">Body text</p>
```

### Grid Layouts
Components use mobile-first grid stacking:
```tsx
<div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
  {/* Items automatically stack on mobile */}
</div>
```

### Touch-Friendly Targets
All interactive elements maintain minimum 44x44px touch targets for accessibility.

## Pages Optimized for Mobile

### 1. Innovation Hub (`/innovation-hub`)
- **Mobile**: Single column layout with stacked cards
- **Tablet**: Two-column layout
- **Desktop**: Multi-column dashboard
- Features: Responsive grid stats, mobile-friendly tabs

### 2. Designer (`/designer`)
- **Mobile**: Stacked BPMN editor + tools
- **Tablet**: Side-by-side layout
- **Desktop**: Full-screen editor with sidebar
- Features: Touch-friendly canvas controls, responsive dialogs

### 3. Cases (`/cases`)
- **Mobile**: Vertical table layout, card view option
- **Tablet**: Condensed table
- **Desktop**: Full data table
- Features: Swipeable cards, filterable list

### 4. Collaboration (`/collaboration`)
- **Mobile**: Single column (members, chat, activity stacked)
- **Tablet**: Two-column layout
- **Desktop**: Three-column layout
- Features: Touch-friendly chat input, responsive member list

### 5. Analytics (`/analytics`)
- **Mobile**: Stacked metrics and charts
- **Tablet**: Two-column layout
- **Desktop**: Full dashboard
- Features: Responsive tables, touch-friendly data visualization

## Mobile Utilities

Use the `lib/mobile-optimizations.ts` utilities for responsive behavior:

```typescript
import {
  isMobileDevice,
  isTouchDevice,
  isSmallScreen,
  formatMobileNumber,
  formatMobileDate,
  getMobilePadding,
  getMobileGap,
} from '@/lib/mobile-optimizations'

// Detect device type
if (isMobileDevice()) {
  // Use mobile-specific behavior
}

// Format numbers for mobile display
const display = formatMobileNumber(1234567) // "1.2M"

// Get responsive spacing
const padding = getMobilePadding()
// { container: 'px-4 py-3 md:px-6 md:py-4', ... }
```

## Touch Interactions

### Swipe Support
For future swipe gestures, use `react-use-gesture` or similar:
```tsx
import { useGesture } from '@use-gesture/react'

const bind = useGesture({
  onSwipe: ({ direction: [dx, dy] }) => {
    // Handle swipe
  },
})
```

### Long Press
Implement long-press for mobile context menus:
```tsx
const [isPressed, setIsPressed] = useState(false)
const timeoutRef = useRef<NodeJS.Timeout>()

const handleMouseDown = () => {
  timeoutRef.current = setTimeout(() => {
    setIsPressed(true)
    // Show context menu
  }, 500)
}

const handleMouseUp = () => {
  clearTimeout(timeoutRef.current)
  setIsPressed(false)
}
```

## Performance Optimizations

### Image Optimization
- Use Next.js Image component for automatic optimization
- Provide mobile-sized variants
```tsx
import Image from 'next/image'

<Image
  src="/image.jpg"
  alt="Description"
  responsive
  sizes="(max-width: 768px) 100vw, 50vw"
/>
```

### Lazy Loading
- Use `useCallback` and `useMemo` to prevent unnecessary re-renders
- Implement virtualization for long lists
```tsx
import { FixedSizeList } from 'react-window'

<FixedSizeList
  height={600}
  itemCount={1000}
  itemSize={50}
>
  {Row}
</FixedSizeList>
```

### Bundle Size
- Code split routes using dynamic imports
```tsx
const DesignerPage = dynamic(() => import('./designer'), {
  loading: () => <LoadingSpinner />,
})
```

## Viewport Meta Tags

All pages include proper meta tags in `app/layout.tsx`:
```html
<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=5" />
```

## Safe Areas (Notch Support)

For devices with notches:
```css
padding: max(1rem, env(safe-area-inset-top)) max(1rem, env(safe-area-inset-right)) max(1rem, env(safe-area-inset-bottom)) max(1rem, env(safe-area-inset-left));
```

## Testing Mobile Responsiveness

### Browser DevTools
1. Open Chrome DevTools
2. Press Ctrl+Shift+M (Cmd+Shift+M on Mac)
3. Select device type and test

### Devices to Test
- iPhone SE (375px)
- iPhone 12/13 (390px)
- iPad (768px)
- iPad Pro (1024px)

### Testing Checklist
- [ ] All text readable without zooming
- [ ] Buttons/links easily tappable (44x44px minimum)
- [ ] No horizontal scrolling (except full-screen content)
- [ ] Images scale properly
- [ ] Forms are easy to fill
- [ ] Navigation is accessible
- [ ] Performance acceptable on 3G (Lighthouse)

## Accessibility on Mobile

### Touch Targets
- Minimum size: 44x44px
- Spacing: 8px minimum between targets

### Text Legibility
- Minimum font size: 16px on inputs (prevents auto-zoom)
- Line height: 1.5+
- Color contrast: WCAG AA (4.5:1 for text)

### Semantic HTML
- Use proper heading hierarchy
- Provide alt text for images
- Label form inputs explicitly

## Future Enhancements

1. **Progressive Web App (PWA)**
   - Offline support with Service Workers
   - App-like installation

2. **Native Mobile Apps**
   - React Native implementation
   - Push notifications
   - Biometric authentication

3. **Gesture Support**
   - Pinch to zoom on diagrams
   - Swipe navigation
   - Pull-to-refresh

4. **Responsive Charts**
   - Mobile-friendly data visualization
   - Touch-enabled interactions

## Resources

- [Tailwind Responsive Design](https://tailwindcss.com/docs/responsive-design)
- [Mobile Web Best Practices](https://web.dev/mobile/)
- [Responsive Design Testing](https://www.responsivedesignchecker.com/)
- [Web Accessibility WCAG](https://www.w3.org/WAI/WCAG21/quickref/)
